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
-module(eimp_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, get_pool_size/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================
init([]) ->
    Workers = lists:map(
		fun(I) ->
			Name = get_proc_name(I),
			{Name, {eimp_worker, start_link, [Name, I]},
			 permanent, 5000, worker, [eimp_worker]}
		end, lists:seq(1, get_pool_size())),
    Limit = {eimp_limit, {eimp_limit, start_link, []},
	     permanent, 5000, worker, [eimp_limit]},
    {ok, {{one_for_one, 10, 1}, [Limit|Workers]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
get_proc_name(I) ->
    list_to_atom("eimp_worker_" ++ integer_to_list(I)).

-spec get_pool_size() -> pos_integer().
get_pool_size() ->
    try erlang:system_info(logical_processors)
    catch _:_ -> 1
    end.
