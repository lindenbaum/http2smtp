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
%%% A simple service that allows mail delivery of HTTP POSTed content. The
%%% allowed content types are either `application/x-www-form-urlencoded' or
%%% `multipart/form-data'. While URL-encoded uploads can only handle one file
%%% attachment the multipart variant allows an unlimited number of attachments.
%%%
%%% This module implements the application as well as the top level supervisor.
%%% @end
%%%=============================================================================

-module(http2smtp).

-behaviour(application).
-behaviour(supervisor).

%% Application callbacks
-export([start/2, stop/1]).

%% supervisor callbacks
-export([init/1]).

-define(PORT, 8080).
-define(HOST_MATCH, '_').

%%%=============================================================================
%%% Application callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
start(_StartType, _StartArgs) -> supervisor:start_link(?MODULE, []).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
stop(_State) -> cowboy:stop_listener(http).

%%%=============================================================================
%%% supervisor callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
init([]) ->
    {Paths, Acceptors} = http2smtp_http:path_matches(),
    HostMatch = application:get_env(?MODULE, host_match, ?HOST_MATCH),
    Env = [{dispatch, cowboy_router:compile([{HostMatch, Paths}])}],
    Port = application:get_env(?MODULE, http_port, ?PORT),
    error_logger:info_msg("Listening on HTTP port ~w~n", [Port]),
    ProtoOpts = [{compress, true}, {env, Env}],
    {ok, _} = cowboy:start_http(http, Acceptors, [{port, Port}], ProtoOpts),
    {ok, {{one_for_one, 5, 1}, [worker(http2smtp_rate, [])]}}.

%%%=============================================================================
%%% internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
worker(M, As) -> {M, {M, start_link, As}, permanent, brutal_kill, worker, [M]}.
