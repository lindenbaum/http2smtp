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
%%% A server implementing some simple statistic counters.
%%% @end
%%%=============================================================================

-module(http2smtp_stats).

-behaviour(gen_server).

%% API
-export([record/1,
         to_string/0]).

%% Internal API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_cast/2,
         handle_call/3,
         handle_info/2,
         code_change/3,
         terminate/2]).

-record(state, {
          start_time   :: erlang:timestamp(),
          requests = 0 :: non_neg_integer(),
          codes = []   :: [{pos_integer(), non_neg_integer()}]}).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Record a HTTP response's status code.
%% @end
%%------------------------------------------------------------------------------
-spec record(pos_integer()) -> ok.
record(StatusCode) -> gen_server:cast(?MODULE, {record, StatusCode}).

%%------------------------------------------------------------------------------
%% @doc
%% Return a printable iolist of the server's statistics.
%% @end
%%------------------------------------------------------------------------------
-spec to_string() -> iolist().
to_string() ->
    State = #state{} = gen_server:call(?MODULE, get),
    Uptime = to_secs(os:timestamp()) - to_secs(State#state.start_time),
    [io_lib:format("Server uptime: ~s~n", [format_time(Uptime)]),
     io_lib:format("Handled requests: ~w~n", [State#state.requests]),
     [io_lib:format("Requests completed with ~w: ~w~n", [Code, Num])
      || {Code, Num} <- lists:sort(State#state.codes)]].

%%%=============================================================================
%%% Internal API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%% Simply start the server (registered).
%%------------------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
init([]) -> {ok, #state{start_time = os:timestamp()}}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_call(get, _From, State) ->
    {reply, State, State};
handle_call(Request, From, State) ->
    error_logger:error_msg("Unexpected call ~w from ~w~n", [Request, From]),
    {reply, undef, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_cast({record, StatusCode}, State) ->
    {noreply, update_stats(StatusCode, State)};
handle_cast(Request, State) ->
    error_logger:error_msg("Unexpected cast ~w~n", [Request]),
    {noreply, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_info(Info, State) ->
    error_logger:info_msg("Unexpected info ~w~n", [Info]),
    {noreply, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
terminate(_Reason, _State) -> ok.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
update_stats(Code, State = #state{requests = R, codes = Cs}) ->
    NewCs = case lists:keytake(Code, 1, Cs) of
                {value, {Code, N}, Rest} -> [{Code, N + 1} | Rest];
                false                    -> [{Code, 1} | Cs]
            end,
    State#state{requests = R + 1, codes = NewCs}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
to_secs({Mega, Secs, _Micro}) -> Mega * 1000000 + Secs.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
format_time(Secs) ->
    S = Secs rem 60,
    M = (Secs div 60) rem 60,
    H = ((Secs div 60) div 60) rem 24,
    D = ((Secs div 60) div 60) div 24,
    io_lib:format("~w Days ~w Hours ~w Minutes ~w Seconds", [D, H, M, S]).
