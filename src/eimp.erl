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
-module(eimp).

%% API
-export([start/0, stop/0, convert/2, format_error/1, get_type/1]).

-type img_type() :: png | jpeg | webp.
-type error_reason() :: unsupported_format |
			timeout |
			disconnected |
			encode_failure |
			decode_failure.

-export_type([img_type/0, error_reason/0]).

%%%===================================================================
%%% API
%%%===================================================================
start() ->
    application:start(?MODULE).

stop() ->
    application:stop(?MODULE).

-spec convert(binary(), img_type()) -> {ok, binary()} | {error, error_reason()}.
convert(Data, To) ->
    ToCode = code(To),
    case get_type(Data) of
	unknown ->
	    {error, unsupported_format};
	Type ->
	    FromCode = code(Type),
	    Cmd = <<FromCode, ToCode, Data/binary>>,
	    case eimp_worker:call(Cmd) of
		{ok, <<ResCode, Reply/binary>>} ->
		    if ResCode == $0 ->
			    {ok, Reply};
		       true ->
			    {error, erlang:binary_to_atom(Reply, latin1)}
		    end;
		{error, _} = Err ->
		    Err
	    end
    end.

-spec format_error(error_reason()) -> binary().
format_error(unsupported_format) ->
    <<"Unsupported format">>;
format_error(encode_failure) ->
    <<"Encoding error">>;
format_error(decode_failure) ->
    <<"Decoding error">>;
format_error(timeout) ->
    <<"Timeout">>;
format_error(disconnected) ->
    <<"Failed to connect to external eimp process">>.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec get_type(binary()) -> img_type() | unknown.
get_type(<<137, "PNG\r\n", 26, $\n, _/binary>>) ->
    png;
get_type(<<255, X, _/binary>>) when X /= 0 andalso X /= 255 ->
    jpeg;
get_type(<<"RIFF", _:32, "WEBP", _/binary>>) ->
    webp;
get_type(_) ->
    unknown.

-spec code(img_type()) -> char().
code(png) -> $p;
code(jpeg) -> $j;
code(webp) -> $w.
