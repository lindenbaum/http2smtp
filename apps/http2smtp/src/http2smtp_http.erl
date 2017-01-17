%%%=============================================================================
%%%
%%%               |  o __   _|  _  __  |_   _       _ _   (TM)
%%%               |_ | | | (_| (/_ | | |_) (_| |_| | | |
%%%
%%% Copyright (c) 2016 Lindenbaum GmbH
%%%
%%% Permission to use, copy, modify, and/or distribute this software for any
%%% purpose with or without fee is hereby granted, provided that the above
%%% copyright notice and this permission notice appear in all copies.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
%%%
%%% @doc
%%% This module contains the main application logic implemented as cowboy
%%% HTTP handler.
%%% @end
%%%=============================================================================

-module(http2smtp_http).

-behaviour(cowboy_http_handler).

%% API
-export([path_matches/0]).

%% cowboy_http_handler callbacks
-export([init/3,
         handle/2,
         terminate/3]).

-define(BODY_OPTS, [{length, 8*1024*1024}, {read_length, 8*1024*1024}]).
-define(CC, []).
-define(FROM, list_to_binary("http2smtp@" ++ element(2, inet:gethostname()))).
-define(RATE_LIMIT, 2).
-define(SUBJECT, <<"http2smtp">>).
-define(TIMEZONE, auto).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Returns the path matches for this cowboy handler.
%% @end
%%------------------------------------------------------------------------------
-spec path_matches() -> {[tuple()], pos_integer()}.
path_matches() ->
    Env = application:get_all_env(http2smtp),
    Opts = exclude(Env, [included_applications]),
    RateLimit = proplists:get_value(rate_limit, Opts, ?RATE_LIMIT),
    {[{"/[:context]", ?MODULE, Opts}], RateLimit}.

%%%=============================================================================
%%% cowboy_http_handler callbacks
%%%=============================================================================

-record(state, {
          body_opts  :: proplists:proplist(),
          cc         :: [binary()],
          context    :: binary(),
          from       :: binary(),
          rate_limit :: pos_integer(),
          smtp_opts  :: proplists:proplist(),
          subject    :: binary(),
          timezone   :: string() | auto,
          to         :: binary()}).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
init({tcp, _}, Req, Opts) ->
    {{IP, Port}, NewReq} = cowboy_req:peer(Req),
    log("Handling request from ~w.~w.~w.~w:~w", tuple_to_list(IP) ++ [Port]),
    {Context, NextReq} = cowboy_req:binding(context, NewReq, <<>>),
    log("Request context is /~s", [Context]),
    {To, CC} = get_to_and_cc(Context, Opts),
    {ok, NextReq,
     #state{
        body_opts = proplists:get_value(body_opts, Opts, ?BODY_OPTS),
        cc = CC,
        context = Context,
        from = proplists:get_value(from, Opts, ?FROM),
        rate_limit = proplists:get_value(rate_limit, Opts, ?RATE_LIMIT),
        smtp_opts = [_ | _] = proplists:get_value(smtp_opts, Opts),
        subject = proplists:get_value(subject, Opts, ?SUBJECT),
        timezone = proplists:get_value(timezone, Opts, ?TIMEZONE),
        to = To}}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle(Req, State = #state{context = Context, rate_limit = Limit}) ->
    {ok, case http2smtp_rate:exceeds(Context, Limit) of
             false -> handle_request(Req, State);
             true  -> reply(429, Req, "Rate limit exceeded on /~s~n", [Context])
         end, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
terminate(_Reason, _Req, #state{}) -> ok.

%%%=============================================================================
%%% internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_request(Req, State) ->
    case cowboy_req:method(Req) of
        {<<"POST">>, NewReq} -> handle_post(NewReq, State);
        {M, NewReq}          -> reply(405, NewReq, "Invalid method ~s~n", [M])
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_post(Req, State) ->
    case cowboy_req:parse_header(<<"content-type">>, Req) of
        {ok, {<<"application">>, <<"x-www-form-urlencoded">>, []}, NewReq} ->
            handle_urlencoded(NewReq, State);
        {ok, {<<"multipart">>, <<"form-data">>, _}, NewReq} ->
            handle_multipart(NewReq, State);
        Other ->
            reply(400, Req, "Failed to parse content type ~w~n", [Other])
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_urlencoded(Req, State = #state{body_opts = Opts}) ->
    {ok, Query, NewReq} = cowboy_req:body_qs(Req, Opts),
    From = proplists:get_value(<<"from">>, Query, State#state.from),
    Subject = proplists:get_value(<<"subject">>, Query, State#state.subject),
    Body = to_body(proplists:get_value(<<"body">>, Query)),
    Filename = proplists:get_value(<<"filename">>, Query, <<"untitled">>),
    ContentType = <<"application/octet-stream">>,
    Data = proplists:get_value(<<"data">>, Query, <<>>),
    Attachments = normalize_attachments([{Filename, ContentType, Data}]),
    send(From, Subject, Body, Attachments, NewReq, State).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_multipart(Req, State = #state{from = From, subject = Subject}) ->
    handle_multipart(Req, {From, Subject, undefined, []}, State).
handle_multipart(Req, {From, Subject, Body, Files}, State) ->
    case cowboy_req:part(Req) of
        {ok, Hdrs, NewReq} ->
            Opts = State#state.body_opts,
            FormData = cow_multipart:form_data(Hdrs),
            {ok, Data, NextReq} = cowboy_req:part_body(NewReq, Opts),
            Acc = case FormData of
                      {file, _, Filename, ContentType, _} ->
                          File = {Filename, ContentType, Data},
                          {From, Subject, Body, [File | Files]};
                      {data, <<"from">>} ->
                          {Data, Subject, Body, Files};
                      {data, <<"subject">>} ->
                          {From, Data, Body, Files};
                      {data, <<"body">>} ->
                          {From, Subject, Data, Files};
                      {data, Field} ->
                          log("Ignoring field ~s with data ~p~n", [Field, Data]),
                          {From, Subject, Body, Files}
                  end,
            handle_multipart(NextReq, Acc, State);
        {done, NewReq} ->
            Attachments = normalize_attachments(lists:reverse(Files)),
            send(From, Subject, to_body(Body), Attachments, NewReq, State)
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
send(_From, ?SUBJECT, [], [], Req, _State) ->
    reply(400, Req, "Refusing request without body or attachments", []);
send(From, Subject, Body, Attachments, Req, State) ->
    Hdrs = [
            {<<"From">>, From},
            {<<"To">>, State#state.to},
            {<<"Subject">>, Subject},
            {<<"Message-ID">>, message_id()},
            {<<"Date">>, datestr(State#state.timezone)},
            {<<"MIME-Version">>, <<"1.0">>},
            {<<"X-Mailer">>, <<"http2smtp">>}
           ] ++ to_cc(State),
    Att = [{<<"multipart">>, <<"alternative">>, [], [], Body} | Attachments],
    Mail = mimemail:encode({<<"multipart">>, <<"mixed">>, Hdrs, [], Att}, []),
    case gen_smtp_client:send_blocking(
           {From, [State#state.to], Mail},
           State#state.smtp_opts) of
        {error, Reason} ->
            reply(500, Req, "Failed to send mail: ~p~n", [Reason]);
        {error, Reason, Failure} ->
            reply(500, Req, "Failed to send mail: ~p~n", [{Reason, Failure}]);
        Receipt when is_binary(Receipt) ->
            reply(204, Req, "Mail sent successfully: ~s", [Receipt])
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
normalize_attachments(Files) ->
    normalize_attachments(Files, []).
normalize_attachments([], Acc) ->
    lists:reverse(Acc);
normalize_attachments([{Filename, ContentType, <<>>} | Rest], Acc) ->
    log("File ~s of type ~s is empty~n", [Filename, ContentType]),
    normalize_attachments(Rest, Acc);
normalize_attachments([{Filename, ContentType, Data} | Rest], Acc) ->
    case binary:split(ContentType, <<"/">>, [trim]) of
        [<<"application">>, <<"octet-stream">>] ->
            {Type, SubType} = guess_content_type(Filename),
            log("Including file ~s of type ~s/~s~n", [Filename, Type, SubType]),
            normalize_attachments(
              Rest, [to_attachment(Type, SubType, Filename, Data) | Acc]);
        [Type, SubType] when Type =/= <<>>, SubType =/= <<>> ->
            log("Including file ~s of type ~s~n", [Filename, ContentType]),
            normalize_attachments(
              Rest, [to_attachment(Type, SubType, Filename, Data) | Acc]);
        _ ->
            log("Ignoring file ~s of type ~s~n", [Filename, ContentType]),
            normalize_attachments(Rest, Acc)
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
to_attachment(Type, SubType, Filename, Data) ->
    {Type, SubType, [],
     [
      {<<"transfer-encoding">>, <<"base64">>},
      {<<"disposition">>, <<"attachment">>},
      {<<"disposition-params">>, [{<<"filename">>, Filename}]}
     ], Data}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
to_body(undefined) ->
    [];
to_body(<<>>) ->
    [];
to_body(Text) ->
    [{<<"text">>, <<"plain">>, [],
      [
       {<<"content-type-params">>, [ {<<"charset">>, <<"utf-8">>} ]},
       {<<"disposition">>, <<"inline">>},
       {<<"transfer-encoding">>, <<"quoted-printable">>},
       {<<"disposition-params">>, []}
      ], Text}].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
to_cc(#state{cc = []}) -> [];
to_cc(#state{cc = CC}) -> [{<<"Cc">>, CC}].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
reply(StatusCode, Req, Fmt, Args) ->
    log(Fmt, Args),
    {ok, NewReq} = cowboy_req:reply(StatusCode, Req),
    NewReq.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
log(Fmt, Args) -> error_logger:info_msg("~w: " ++ Fmt, [self() | Args]).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_to_and_cc(Context, Opts) ->
    ContextCfg = proplists:get_value(Context, Opts, []),
    To = get_value(to, ContextCfg, Opts, undefined),
    true = is_mail(To),
    CC = get_value(cc, ContextCfg, Opts, ?CC),
    true = lists:all(fun is_mail/1, CC),
    {To, CC}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
is_mail(B) when is_binary(B) -> length([true || <<"@">> <= B]) =:= 1.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
message_id() -> integer_to_binary(crypto:rand_uniform(100000000, 999999999)).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
datestr(Timezone) ->
    qdate:to_string(<<"D, j M Y G:i:s O">>, Timezone, os:timestamp()).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
guess_content_type(Filename) ->
    {Type, SubType, _} = cow_mimetypes:all(Filename),
    {Type, SubType}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_value(Key, L1, L2, Default) ->
    proplists:get_value(Key, L1, proplists:get_value(Key, L2, Default)).

%%------------------------------------------------------------------------------
%% @private
%% Exclude entries with certain keys from a proplist.
%%------------------------------------------------------------------------------
exclude(Proplist, Keys) ->
    [E || E = {K, _} <- Proplist, not lists:member(K, Keys)].
