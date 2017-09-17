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
-module(eimp_test).

-include_lib("eunit/include/eunit.hrl").

test_dir() ->
    {ok, Cwd} = file:get_cwd(),
    filename:join(filename:dirname(Cwd), "test").

start_test() ->
    ?assertEqual(ok, eimp:start()).

png_to_jpeg_test() ->
    convert(png, jpeg).

png_to_webp_test() ->
    convert(png, webp).

jpeg_to_png_test() ->
    convert(jpeg, png).

jpeg_to_webp_test() ->
    convert(jpeg, webp).

webp_to_png_test() ->
    convert(webp, png).

webp_to_jpeg_test() ->
    convert(webp, jpeg).

unsupported_format_test() ->
    ?assertEqual({error, unsupported_format},
		 eimp:convert(<<1,2,3>>, png)).

malformed_png_test() ->
    convert_malformed(png).

malformed_jpeg_test() ->
    convert_malformed(jpeg).

malformed_webp_test() ->
    convert_malformed(webp).

webp_identify_test() ->
    identify(webp, 1024, 772).

jpeg_identify_test() ->
    identify(jpeg, 1024, 772).

png_identify_test() ->
    identify(png, 1024, 772).

too_big_test() ->
    FileName = "spark.png.zip",
    Path = filename:join(test_dir(), FileName),
    {ok, [{_, Data}]} = zip:unzip(Path, [memory]),
    ?assertEqual({error, image_too_big}, eimp:convert(Data, jpeg)).

timeout_test() ->
    ?assertEqual({error, timeout}, eimp_worker:call(<<>>, 0)).

port_restart_test() ->
    ?assertEqual({error, disconnected}, eimp_worker:call(<<>>)).

format_error_test() ->
    lists:foreach(
      fun(Reason) ->
	      ?assertMatch(<<_/binary>>, eimp:format_error(Reason))
      end,
      [unsupported_format,
       timeout,
       disconnected,
       encode_failure,
       decode_failure]).

stop_test() ->
    ?assertEqual(ok, eimp:stop()).

disconnected_test() ->
    {ok, Cwd} = file:get_cwd(),
    Bin = filename:join([Cwd, "priv", "bin", "eimp"]),
    ?assertEqual(ok, file:delete(Bin)),
    start_test(),
    ?assertEqual({error, disconnected}, eimp_worker:call(<<>>)).

convert(From, To) ->
    FileName = "img." ++ atom_to_list(From),
    Path = filename:join(test_dir(), FileName),
    {ok, In} = file:read_file(Path),
    {ok, Out} = eimp:convert(In, To),
    ?assertEqual(To, eimp:get_type(Out)).

convert_malformed(From) ->
    FileName = "img." ++ atom_to_list(From),
    Path = filename:join(test_dir(), FileName),
    {ok, <<In:1024/binary, _/binary>>} = file:read_file(Path),
    ?assertEqual({error, decode_failure}, eimp:convert(In, png)).

identify(From, W, H) ->
    FileName = "img." ++ atom_to_list(From),
    Path = filename:join(test_dir(), FileName),
    {ok, In} = file:read_file(Path),
    ?assertEqual({ok, [{type, From}, {width, W}, {height, H}]},
		 eimp:identify(In)).
