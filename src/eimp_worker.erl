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
-module(eimp_worker).

-behaviour(gen_server).

%% API
-export([start_link/2, call/1, call/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(MAX_RETRIES, 2).
-define(CALL_TIMEOUT, 30000).

-record(state, {port :: undefined | port(),
		links = sets:new() :: sets:set(),
		os_pid :: undefined | pos_integer(),
		num :: pos_integer()}).

%%%===================================================================
%%% API
%%%===================================================================
start_link(Name, I) ->
    gen_server:start_link({local, Name}, ?MODULE, [I], []).

-spec call(binary()) -> {ok, binary()} | {error, eimp:error_reason()}.
call(Data) ->
    call(Data, ?CALL_TIMEOUT).

-spec call(binary(), non_neg_integer()) ->
		  {ok, binary()} | {error, eimp:error_reason()}.
call(Data, Timeout) ->
    case eimp:is_gd_compiled() of
	true ->
	    StartTime = p1_time_compat:monotonic_time(milli_seconds),
	    PoolSize = eimp_sup:get_pool_size(),
	    I = p1_time_compat:unique_integer([positive, monotonic]),
	    Tag = term_to_binary(self()),
	    Cmd = <<(size(Tag)), Tag/binary, Data/binary>>,
	    do_call(Cmd, I, I + PoolSize, PoolSize, StartTime + Timeout, StartTime);
	false ->
	    {error, unsupported_format}
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([I]) ->
    process_flag(trap_exit, true),
    {Port, OSPid} = case eimp:is_gd_compiled() of
			true -> start_port(I);
			false -> {undefined, undefined}
		    end,
    {ok, #state{port = Port, os_pid = OSPid, num = I}}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({Port, {data, Data}}, State) ->
    <<Len, Tag:Len/binary, Reply/binary>> = Data,
    Pid = binary_to_term(Tag),
    Pid ! {Port, Reply},
    {noreply, State};
handle_info({monitor_port, Port, Pid}, State) ->
    if State#state.port == Port ->
	    Links = sets:add_element(Pid, State#state.links),
	    {noreply, State#state{links = Links}};
       true ->
	    Pid ! {'EXIT', Port, normal},
	    {noreply, State}
    end;
handle_info({demonitor_port, Port, Pid}, State) ->
    if State#state.port == Port ->
	    Links = sets:del_element(Pid, State#state.links),
	    {noreply, State#state{links = Links}};
       true ->
	    {noreply, State}
    end;
handle_info({'EXIT', Port, Reason}, #state{port = Port} = State) ->
    error_logger:error_msg("External eimp process (pid=~w) has terminated "
			   "unexpectedly, restarting in a few seconds",
			   [State#state.os_pid]),
    Links = sets:filter(
	      fun(Pid) ->
		      Pid ! {'EXIT', Port, Reason},
		      false
	      end, State#state.links),
    State1 = State#state{port = undefined,
			 os_pid = undefined,
			 links = Links},
    restart_after(),
    {noreply, State1};
handle_info(start_port, #state{port = undefined, num = I} = State) ->
    {Port, OSPid} = start_port(I),
    {noreply, State#state{port = Port, os_pid = OSPid}};
handle_info(Info, State) ->
    error_logger:error_msg("got unexpected info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, #state{port = Port} = State) ->
    if is_port(Port) ->
	    catch port_close(Port),
	    sets:filter(
	      fun(Pid) ->
		      Pid ! {'EXIT', Port, terminated},
		      false
	      end, State#state.links);
       true ->
	    ok
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec do_call(binary(), non_neg_integer(), non_neg_integer(),
	      non_neg_integer(), non_neg_integer(), non_neg_integer()) ->
		     {ok, binary()} | {error, eimp:error_reason()}.
do_call(_, Max, Max, _, _, _) ->
    {error, disconnected};
do_call(Cmd, I, Max, PoolSize, EndTime, CurrTime) ->
    Timeout = EndTime - CurrTime,
    if Timeout > 0 ->
	    Port = get_port((I rem PoolSize) + 1),
	    monitor_port(Port),
	    try
		case port_command(Port, Cmd) of
		    true ->
			receive
			    {Port, Reply} ->
				demonitor_port(Port),
				{ok, Reply};
			    {'EXIT', Port, _} ->
				erlang:error(badarg)
			after Timeout ->
				demonitor_port(Port),
				{error, timeout}
			end;
		    false ->
			erlang:error(badarg)
		end
	    catch _:badarg ->
		    demonitor_port(Port),
		    do_call(Cmd, I+1, Max, PoolSize, EndTime,
			    p1_time_compat:monotonic_time(milli_seconds))
	    end;
       true ->
	    {error, timeout}
    end.

-spec start_port(pos_integer()) -> {port() | undefined, integer() | undefined}.
start_port(I) ->
    Path = filename:join(get_bin_path(), atom_to_list(eimp)),
    case file:open(Path, [read]) of
        {ok, Fd} ->
            file:close(Fd),
	    Port = open_port({spawn_executable, Path}, [{packet, 4}, binary]),
	    Name = get_name(I),
	    try
		link(Port),
		register(Name, Port),
		case erlang:port_info(Port, os_pid) of
		    {os_pid, OSPid} ->
			{Port, OSPid};
		    undefined ->
			{Port, undefined}
		end
	    catch _:badarg ->
		    flush_queue(Port),
		    restart_after(),
		    {undefined, undefined}
	    end;
        {error, Why} ->
            error_logger:error_msg(
	      "Failed to read ~s: ~s", [Path, file:format_error(Why)]),
	    restart_after(),
	    {undefined, undefined}
    end.

-spec monitor_port(port() | undefined) -> ok.
monitor_port(Port) ->
    case erlang:port_info(Port, connected) of
	{connected, Pid} ->
	    Pid ! {monitor_port, Port, self()};
	undefined ->
	    self() ! {'EXIT', Port, normal}
    end,
    ok.

-spec demonitor_port(port() | undefined) -> ok.
demonitor_port(Port) ->
    case erlang:port_info(Port, connected) of
	{connected, Pid} ->
	    Pid ! {demonitor_port, Port, self()};
	undefined ->
	    ok
    end,
    flush_queue(Port).

-spec get_bin_path() -> file:filename().
get_bin_path() ->
    EbinDir = filename:dirname(code:which(eimp)),
    AppDir = filename:dirname(EbinDir),
    filename:join([AppDir, "priv", "bin"]).

-spec get_port(pos_integer()) -> undefined | port().
get_port(I) ->
    whereis(get_name(I)).

-spec get_name(pos_integer()) -> atom().
get_name(I) ->
    list_to_atom("eimp_port_" ++ integer_to_list(I)).

-ifdef(RAND_UNIFORM).
get_reconnect_timeout() ->
    Num = eimp_sup:get_pool_size(),
    rand:uniform(timer:seconds(Num)).
-else.
get_reconnect_timeout() ->
    Num = eimp_sup:get_pool_size(),
    crypto:rand_uniform(1, timer:seconds(Num)).
-endif.

-spec restart_after() -> reference().
restart_after() ->
    erlang:send_after(get_reconnect_timeout(), self(), start_port).

-spec flush_queue(port() | undefined) -> ok.
flush_queue(Port) when is_port(Port) ->
    receive {'EXIT', Port, _} -> ok
    after 0 -> ok
    end;
flush_queue(_) ->
    ok.
