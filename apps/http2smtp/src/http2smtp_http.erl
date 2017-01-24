%%%=============================================================================
%%%
%%%               |  o __   _|  _  __  |_   _       _ _   (TM)
%%%               |_ | | | (_| (/_ | | |_) (_| |_| | | |
%%%
%%% Copyright (c) 2017 Lindenbaum GmbH
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
    Limits = get_values(rate_limit, Opts, ?RATE_LIMIT),
    RateLimit = lists:foldl(fun erlang:'+'/2, 0, Limits),
    {[{"/[:context]", ?MODULE, Opts}], RateLimit}.

%%%=============================================================================
%%% cowboy_http_handler callbacks
%%%=============================================================================

-record(state, {
          allowed    :: [binary()],
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
    ContextOpts = get_value(Context, Opts, []),
    {ok, NextReq,
     #state{
        allowed = to_binlist(get_value(allowed_content_types, Opts, [])),
        body_opts = get_value(body_opts, Opts, ?BODY_OPTS),
        cc = get_cc(ContextOpts, Opts),
        context = Context,
        from = to_bin(get_value(from, Opts, ?FROM)),
        rate_limit = get_value(rate_limit, ContextOpts, Opts, ?RATE_LIMIT),
        smtp_opts = [_ | _] = get_value(smtp_opts, Opts, undefined),
        subject = to_bin(get_value(subject, Opts, ?SUBJECT)),
        timezone = get_value(timezone, Opts, ?TIMEZONE),
        to = get_to(ContextOpts, Opts)}}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle(Req, State = #state{context = Context}) ->
    {Method, NewReq} = cowboy_req:method(Req),
    {ok, case Method of
             <<"GET">> when Context =:= <<"status">> ->
                 reply_status(NewReq);
             <<"POST">> ->
                 handle_post(NewReq, State);
             Method ->
                 reply(405, NewReq, "Invalid method ~s~n", [Method])
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
handle_urlencoded(Req, State = #state{allowed = Allowed, body_opts = Opts}) ->
    {ok, Query, NewReq} = cowboy_req:body_qs(Req, Opts),
    From = get_value(<<"from">>, Query, State#state.from),
    Subject = get_value(<<"subject">>, Query, State#state.subject),
    Body = to_body(get_value(<<"body">>, Query, undefined)),
    Filename = get_value(<<"filename">>, Query, <<"untitled">>),
    ContentType = <<"application/octet-stream">>,
    Data = get_value(<<"data">>, Query, <<>>),
    Files = [{filename:basename(Filename), ContentType, Data}],
    Attachments = normalize_attachments(Allowed, Files),
    handle_mail(From, Subject, Body, Attachments, NewReq, State).

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
                          FName = filename:basename(Filename),
                          File = {FName, ContentType, Data},
                          {From, Subject, Body, [File | Files]};
                      {data, <<"from">>} ->
                          {Data, Subject, Body, Files};
                      {data, <<"subject">>} ->
                          {From, Data, Body, Files};
                      {data, <<"body">>} ->
                          {From, Subject, Data, Files};
                      {data, Field} ->
                          log("Ignoring field ~s~n", [Field]),
                          {From, Subject, Body, Files}
                  end,
            handle_multipart(NextReq, Acc, State);
        {done, NewReq} ->
            Allowed = State#state.allowed,
            Attachments = normalize_attachments(Allowed, lists:reverse(Files)),
            handle_mail(From, Subject, to_body(Body), Attachments, NewReq, State)
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_mail(_From, ?SUBJECT, [], [], Req, _State) ->
    reply(400, Req, "Refusing request without body or attachments", []);
handle_mail(From, Subject, Body, Attachments, Req, State) ->
    Hdrs = [
            {<<"From">>, From}, %% will be verified/encoded by gen_smtp
            {<<"To">>, State#state.to},
            {<<"Subject">>, Subject}, %% will be verified/encoded by gen_smtp
            {<<"Message-ID">>, message_id()},
            {<<"Date">>, datestr(State#state.timezone)},
            {<<"MIME-Version">>, <<"1.0">>},
            {<<"X-Mailer">>, <<"http2smtp">>}
           ] ++ to_cc(State),  %% will be verified/encoded by gen_smtp
    Att = [{<<"multipart">>, <<"alternative">>, [], [], Body} | Attachments],
    Mail = mimemail:encode({<<"multipart">>, <<"mixed">>, Hdrs, [], Att}, []),
    maybe_relay(From, Mail, Req, State).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
maybe_relay(From, Mail, Req, State = #state{context = Context}) ->
    case http2smtp_rate:exceeds(Context, State#state.rate_limit) of
        false -> relay(From, Mail, Req, State);
        true  -> reply(429, Req, "Rate limit exceeded on /~s~n", [Context])
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
relay(From, Mail, Req, #state{to = To, smtp_opts = SmtpOpts}) ->
    case gen_smtp_client:send_blocking({From, [To], Mail}, SmtpOpts) of
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
normalize_attachments(Allowed, Files) ->
    normalize_attachments(Allowed, Files, []).
normalize_attachments(_Allowed, [], Acc) ->
    lists:reverse(lists:flatten(Acc));
normalize_attachments(Allowed, [{Filename, ContentType, <<>>} | Rest], Acc) ->
    log("Skipping empty file ~s of type ~s~n", [Filename, ContentType]),
    normalize_attachments(Allowed, Rest, Acc);
normalize_attachments(Allowed, [{Filename, ContentType, Data} | Rest], Acc) ->
    case binary:split(ContentType, <<"/">>, [trim]) of
        [<<"application">>, <<"octet-stream">>] ->
            {Type, SubType} = guess_content_type(Filename),
            normalize_attachments(
              Allowed,
              Rest,
              [to_attachment(Allowed, Type, SubType, Filename, Data) | Acc]);
        [Type, SubType] when Type =/= <<>>, SubType =/= <<>> ->
            normalize_attachments(
              Allowed,
              Rest,
              [to_attachment(Allowed, Type, SubType, Filename, Data) | Acc]);
        _ ->
            log("Skipping file ~s of type ~s~n", [Filename, ContentType]),
            normalize_attachments(Allowed, Rest, Acc)
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
to_attachment(Allowed, Type, SubType, Filename, Data) ->
    case is_allowed(Type, SubType, Allowed) of
        true ->
            log("Including file ~s of type ~s/~s~n", [Filename, Type, SubType]),
            [{Type, SubType, [],
              [
               {<<"transfer-encoding">>, <<"base64">>},
               {<<"disposition">>, <<"attachment">>},
               {<<"disposition-params">>,
                [
                 {<<"filename">>, Filename}
                ]}
              ], Data}];
        false ->
            log("Skipping file ~s of type ~s/~s~n", [Filename, Type, SubType]),
            []
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
is_allowed(_Type, _SubType, []) ->
    true;
is_allowed(Type, SubType, Allowed) ->
    lists:member(<<Type/binary, $/, SubType/binary>>, Allowed).

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
    ok = http2smtp_stats:record(StatusCode),
    {ok, NewReq} = cowboy_req:reply(StatusCode, Req),
    NewReq.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
reply_status(Req) ->
    Hdrs = [{<<"content-type">>, <<"text/plain">>}],
    Text = iolist_to_binary(http2smtp_stats:to_string()),
    {ok, NewReq} = cowboy_req:reply(200, Hdrs, Text, Req),
    NewReq.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
log(Fmt, Args) -> error_logger:info_msg("~w: " ++ Fmt, [self() | Args]).

%%------------------------------------------------------------------------------
%% @private
%% Checks whether the given input binary looks like an email (roughly), an
%% exact check is performed by gen_smtp. This check is only for configuration
%% verification.
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
%% Convert an iolist to a binary string.
%%------------------------------------------------------------------------------
to_bin(S) -> iolist_to_binary([S]).

%%------------------------------------------------------------------------------
%% @private
%% Convert a list of iolists to a list of binary strings.
%%------------------------------------------------------------------------------
to_binlist(L) -> [to_bin(E) || E <- L].

%%------------------------------------------------------------------------------
%% @private
%% Return the Cc address list either from the given context options or from the
%% global options. Crashes if not set.
%%------------------------------------------------------------------------------
get_cc(ContextOpts, Opts) ->
    CC = to_binlist(get_value(cc, ContextOpts, Opts, ?CC)),
    true = lists:all(fun is_mail/1, CC),
    CC.

%%------------------------------------------------------------------------------
%% @private
%% Return the To address either from the given context options or from the
%% global options. Crashes if not set.
%%------------------------------------------------------------------------------
get_to(ContextOpts, Opts) ->
    To = to_bin(get_value(to, ContextOpts, Opts, undefined)),
    true = is_mail(To),
    To.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_value(Key, L, Default) -> proplists:get_value(Key, L, Default).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_value(Key, L1, L2, Default) ->
    get_value(Key, L1, get_value(Key, L2, Default)).

%%------------------------------------------------------------------------------
%% @private
%% Return a list of values for a given key from a deep proplist. If not a single
%% value is found for the given key, the default value will be returned as
%% single result list entry.
%%------------------------------------------------------------------------------
get_values(Key, L, Default) ->
    get_values(Key, L, [], Default).
get_values(_Key, [], [], Default) ->
    [Default];
get_values(_Key, [], Acc, _Default) ->
    lists:reverse(Acc);
get_values(Key, [{Key, Value} | Rest], Acc, Default) ->
    get_values(Key, Rest, [Value | Acc], Default);
get_values(Key, [{_, Value} | Rest], Acc, Default) when is_list(Value) ->
    get_values(Key, lists:append(Value, Rest), Acc, Default);
get_values(Key, [_ | Rest], Acc, Default) ->
    get_values(Key, Rest, Acc, Default).

%%------------------------------------------------------------------------------
%% @private
%% Exclude entries with certain keys from a proplist.
%%------------------------------------------------------------------------------
exclude(Proplist, Keys) ->
    [E || E = {K, _} <- Proplist, not lists:member(K, Keys)].
