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
%%% A server implementing a minute-based rate limit counter.
%%% @end
%%%=============================================================================

-module(http2smtp_rate).

-behaviour(gen_server).

%% API
-export([exceeds/2]).

%% Internal API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_cast/2,
         handle_call/3,
         handle_info/2,
         code_change/3,
         terminate/2]).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Returns whether the current rate (count) for a given context exceeds the
%% given limit.
%% @end
%%------------------------------------------------------------------------------
-spec exceeds(term(), pos_integer()) -> boolean().
exceeds(Context, Limit) -> gen_server:call(?MODULE, {count, Context}) > Limit.

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

-record(state, {
          timer       :: reference() | undefined,
          counts = [] :: [{term(), non_neg_integer()}]}).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
init([]) -> {ok, schedule_timer(#state{})}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_call({count, Context}, _From, State = #state{counts = Counts}) ->
    {Count, NewCounts} =
        case lists:keytake(Context, 1, Counts) of
            {value, {_, C}, Rest} -> {C + 1, [{Context, C + 1} | Rest]};
            false                 -> {1, [{Context, 1} | Counts]}
        end,
    {reply, Count, State#state{counts = NewCounts}};
handle_call(Request, From, State) ->
    error_logger:error_msg("Unexpected call ~w from ~w~n", [Request, From]),
    {reply, undef, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_cast(Request, State) ->
    error_logger:error_msg("Unexpected cast ~w~n", [Request]),
    {noreply, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_info({timeout, Ref, reset}, State = #state{timer = Ref}) ->
    {noreply, schedule_timer(State#state{counts = []})};
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
schedule_timer(State) ->
    State#state{timer = erlang:start_timer(60000, self(), reset)}.
