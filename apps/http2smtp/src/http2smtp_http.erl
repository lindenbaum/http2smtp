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
-define(FROM, list_to_binary("http2smtp@" ++ element(2, inet:gethostname()))).
-define(PATH_MATCH, "/").
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
    PathMatch = proplists:get_value(path_match, Opts, ?PATH_MATCH),
    RateLimit = proplists:get_value(rate_limit, Opts, ?RATE_LIMIT),
    {[{PathMatch, ?MODULE, Opts}], RateLimit}.

%%%=============================================================================
%%% cowboy_http_handler callbacks
%%%=============================================================================

-record(state, {
          body_opts  :: proplists:proplist(),
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
    {to, To} = {to, <<_/binary>>} = lists:keyfind(to, 1, Opts),
    {ok, Req,
     #state{
        body_opts = proplists:get_value(body_opts, Opts, ?BODY_OPTS),
        from = proplists:get_value(from, Opts, ?FROM),
        rate_limit = proplists:get_value(rate_limit, Opts, ?RATE_LIMIT),
        smtp_opts = filter(Opts, [relay, username, password]),
        subject = proplists:get_value(subject, Opts, ?SUBJECT),
        timezone = proplists:get_value(timezone, Opts, ?TIMEZONE),
        to = To}}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle(Req, State = #state{rate_limit = Limit}) ->
    {ok, case http2smtp_rate:exceeds(Limit) of
             false -> handle_request(Req, State);
             true  -> reply(429, Req, "Rate limit exceeded~n", [])
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
handle_post(Req, State = #state{body_opts = Opts}) ->
    case cowboy_req:part(Req) of
        {ok, Hdrs, NewReq} ->
            {file, _, Filename, ContentType, _} = cow_multipart:form_data(Hdrs),
            case binary:split(ContentType, <<"/">>, [trim]) of
                [Type, SubType] when Type =/= <<>>, SubType =/= <<>> ->
                    {ok, Data, NextReq} = cowboy_req:part_body(NewReq, Opts),
                    handle_file(Filename, Type, SubType, Data, NextReq, State);
                _ ->
                    reply(400, Req, "Cannot handle ~s~n", [ContentType])
            end;
        _ ->
            reply(400, Req, "No multipart content~n", [])
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_file(Filename, Type, SubType, Data, Req, State) ->
    Hdrs = [
            {<<"From">>, State#state.from},
            {<<"To">>, State#state.to},
            {<<"Subject">>, State#state.subject},
            {<<"Message-ID">>, message_id()},
            {<<"Date">>, datestr(State#state.timezone)},
            {<<"MIME-Version">>, <<"1.0">>},
            {<<"X-Mailer">>, <<"http2smtp">>}
           ],
    Att = [
           {<<"multipart">>, <<"alternative">>, [], [], []},
           {Type, SubType, [],
            [
             {<<"transfer-encoding">>, <<"base64">>},
             {<<"disposition">>, <<"attachment">>},
             {<<"disposition-params">>, [{<<"filename">>, Filename}]}
            ], Data}
          ],
    Mail = mimemail:encode({<<"multipart">>, <<"mixed">>, Hdrs, [], Att}, []),
    case gen_smtp_client:send_blocking(
           {State#state.from, [State#state.to], Mail},
           State#state.smtp_opts) of
        {error, Reason} ->
            reply(500, Req, "Failed to send mail: ~w~n", [Reason]);
        {error, Reason, Failure} ->
            reply(500, Req, "Failed to send mail: ~w~n", [{Reason, Failure}]);
        Receipt when is_binary(Receipt) ->
            reply(200, Req, "mail accepted~n", [])
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
reply(StatusCode, Req, Fmt, Args) ->
    error_logger:info_msg(Fmt, Args),
    {ok, NewReq} = cowboy_req:reply(StatusCode, Req),
    NewReq.

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
%% Exclude entries with certain keys from a proplist.
%%------------------------------------------------------------------------------
exclude(Proplist, Keys) ->
    [E || E = {K, _} <- Proplist, not lists:member(K, Keys)].

%%------------------------------------------------------------------------------
%% @private
%% Filter a proplist for certain keys.
%%------------------------------------------------------------------------------
filter(Proplist, Keys) -> [E || E = {K, _} <- Proplist, lists:member(K, Keys)].
