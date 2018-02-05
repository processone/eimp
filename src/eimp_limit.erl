%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2002-2017 ProcessOne, SARL. All Rights Reserved.
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%%-------------------------------------------------------------------
-module(eimp_limit).

-behaviour(gen_server).

%% API
-export([start_link/0, is_blocked/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(LIMIT_PERIOD, 60*1000*1000).

-record(state, {limits = treap:empty() :: treap:treap()}).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

is_blocked(undefined, _) ->
    false;
is_blocked(_, undefined) ->
    false;
is_blocked(Limiter, RateLimit) when is_integer(RateLimit), RateLimit > 0 ->
    gen_server:call(?MODULE, {is_blocked, Limiter, RateLimit}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    {ok, #state{}}.

handle_call({is_blocked, Limiter, RateLimit}, _From, State) ->
    NowPriority = now_priority(),
    CleanPriority = NowPriority + ?LIMIT_PERIOD,
    Limits = clean_treap(State#state.limits, CleanPriority),
    case treap:lookup(Limiter, Limits) of
	{ok, _, Rate} when Rate >= RateLimit ->
	    {reply, true, State#state{limits = Limits}};
	{ok, Priority, Rate} ->
	    NewLimits = treap:insert(Limiter, Priority, Rate + 1, Limits),
	    {reply, false, State#state{limits = NewLimits}};
	_ ->
	    NewLimits = treap:insert(Limiter, NowPriority, 1, Limits),
	    {reply, false, State#state{limits = NewLimits}}
    end;
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
clean_treap(Treap, CleanPriority) ->
    case treap:is_empty(Treap) of
	true -> Treap;
	false ->
	    {_Key, Priority, _Value} = treap:get_root(Treap),
	    if Priority > CleanPriority ->
		    clean_treap(treap:delete_root(Treap), CleanPriority);
	       true ->
		    Treap
	    end
    end.

now_priority() ->
    -p1_time_compat:system_time(micro_seconds).
