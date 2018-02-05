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

is_supported_test() ->
    ?assertEqual(true, eimp:is_supported(webp)),
    ?assertEqual(true, eimp:is_supported(png)),
    ?assertEqual(true, eimp:is_supported(jpeg)),
    ?assertEqual(true, eimp:is_supported(gif)).

supported_formats_test() ->
    ?assertEqual([webp, jpeg, png, gif], eimp:supported_formats()).

png_to_jpeg_test() ->
    convert(png, jpeg).

png_to_webp_test() ->
    convert(png, webp).

png_to_gif_test() ->
    convert(png, gif).

jpeg_to_png_test() ->
    convert(jpeg, png).

jpeg_to_webp_test() ->
    convert(jpeg, webp).

jpeg_to_gif_test() ->
    convert(jpeg, gif).

webp_to_png_test() ->
    convert(webp, png).

webp_to_jpeg_test() ->
    convert(webp, jpeg).

webp_to_gif_test() ->
    convert(webp, gif).

gif_to_png_test() ->
    convert(gif, png).

gif_to_jpeg_test() ->
    convert(gif, jpeg).

%% GIF->WEBP is not supported by libgd, I have no idea why
%% Use this as encode failure check
encode_failure_test() ->
    In = read(gif),
    ?assertEqual({error, encode_failure}, eimp:convert(In, webp)).

scale_png_test() ->
    scale(png).

scale_jpeg_test() ->
    scale(jpeg).

scale_gif_test() ->
    scale(gif).

scale_webp_test() ->
    scale(webp).

convert_unsupported_test() ->
    ?assertEqual({error, unsupported_format},
		 eimp:convert(<<1,2,3>>, png)).

identify_unsupported_test() ->
    ?assertEqual({error, unsupported_format},
		 eimp:identify(<<1,2,3>>)).

malformed_png_test() ->
    convert_malformed(png).

malformed_jpeg_test() ->
    convert_malformed(jpeg).

malformed_webp_test() ->
    convert_malformed(webp).

malformed_gif_test() ->
    convert_malformed(gif).

webp_identify_test() ->
    identify(webp, 1024, 772).

jpeg_identify_test() ->
    identify(jpeg, 1024, 772).

png_identify_test() ->
    identify(png, 1024, 772).

gif_identify_test() ->
    identify(gif, 1024, 772).

too_big_test() ->
    FileName = "spark.png.zip",
    Path = filename:join(test_dir(), FileName),
    {ok, [{_, Data}]} = zip:unzip(Path, [memory]),
    ?assertEqual({error, image_too_big}, eimp:convert(Data, jpeg)).

too_many_requests_test() ->
    From = png,
    To = jpeg,
    Opts = [{limit_by, self()}, {rate_limit, 3}],
    lists:foreach(fun(_) -> convert(From, To, Opts) end, lists:seq(1, 3)),
    In = read(From),
    ?assertEqual({error, too_many_requests}, eimp:convert(In, To, Opts)).

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
       image_too_big,
       timeout,
       disconnected,
       transform_failure,
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
    convert(From, To, []).

convert(From, To, Opts) ->
    In = read(From),
    {ok, Out} = eimp:convert(In, To, Opts),
    ?assertEqual(To, eimp:get_type(Out)).

convert_malformed(From) ->
    <<In:100/binary, _/binary>> = read(From),
    ?assertEqual({error, decode_failure}, eimp:convert(In, png)).

identify(From, W, H) ->
    In = read(From),
    ?assertEqual({ok, [{type, From}, {width, W}, {height, H}]},
		 eimp:identify(In)).

scale(From) ->
    In = read(From),
    {ok, Out} = eimp:convert(In, From, [{scale, {512, 386}}]),
    ?assertEqual({ok, [{type, From}, {width, 512}, {height, 386}]},
		 eimp:identify(Out)).

read(Format) ->
    FileName = "img." ++ atom_to_list(Format),
    Path = filename:join(test_dir(), FileName),
    {ok, Data} = file:read_file(Path),
    Data.
