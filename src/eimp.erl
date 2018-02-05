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
-export([start/0, stop/0, convert/2, convert/3, identify/1, format_error/1,
	 get_type/1, supported_formats/0, is_supported/1]).
%% For internal needs only
-export([is_gd_compiled/0]).

-type img_type() :: png | jpeg | webp | gif.
-type error_reason() :: unsupported_format |
			timeout |
			disconnected |
			encode_failure |
			decode_failure |
			transform_failure |
			too_many_requests |
			image_too_big.
-type info() :: [{type, img_type()} |
		 {width, non_neg_integer()} |
		 {height, non_neg_integer()}].
-type width() :: non_neg_integer().
-type height() :: non_neg_integer().
-type convert_opts() :: [{scale, {width(), height()}}].

-export_type([img_type/0, error_reason/0, info/0]).

-define(CMD_CONVERT, 1).
-define(CMD_IDENTIFY, 2).

%%%===================================================================
%%% API
%%%===================================================================
start() ->
    application:start(?MODULE).

stop() ->
    application:stop(?MODULE).

-spec convert(binary(), img_type()) -> {ok, binary()} | {error, error_reason()}.
convert(Data, To) ->
    convert(Data, To, []).

-spec convert(binary(), img_type(), convert_opts()) ->
		     {ok, binary()} | {error, error_reason()}.
convert(Data, To, Opts) ->
    ToCode = code(To),
    case get_type(Data) of
	unknown ->
	    {error, unsupported_format};
	Type ->
	    EncOpts = encode_options(Opts),
	    FromCode = code(Type),
	    Cmd = <<?CMD_CONVERT, FromCode, ToCode, EncOpts/binary, Data/binary>>,
	    Limiter = proplists:get_value(limit_by, Opts),
	    RateLimit = proplists:get_value(rate_limit, Opts),
	    case eimp_limit:is_blocked(Limiter, RateLimit) of
		false ->
		    call(Cmd);
		true ->
		    {error, too_many_requests}
	    end
    end.

-spec identify(binary()) -> {ok, info()} | {error, error_reason()}.
identify(Data) ->
    case get_type(Data) of
	unknown ->
	    {error, unsupported_format};
	Type ->
	    FromCode = code(Type),
	    Cmd = <<?CMD_IDENTIFY, FromCode, Data/binary>>,
	    case call(Cmd) of
		{ok, <<W:32, H:32>>} ->
		    {ok, [{type, Type}, {width, W}, {height, H}]};
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
format_error(transform_failure) ->
    <<"Transformation error">>;
format_error(timeout) ->
    <<"Timeout">>;
format_error(disconnected) ->
    <<"Failed to connect to external eimp process">>;
format_error(too_many_requests) ->
    <<"Too many requests">>;
format_error(image_too_big) ->
    <<"Image is too big">>.

-spec is_supported(img_type()) -> boolean().
is_supported(jpeg) ->
    is_jpeg_compiled();
is_supported(png) ->
    is_png_compiled();
is_supported(gif) ->
    is_gd_compiled();
is_supported(webp) ->
    is_webp_compiled();
is_supported(_) ->
    false.

-spec supported_formats() -> [img_type()].
supported_formats() ->
    lists:filter(fun is_supported/1, [webp, jpeg, png, gif]).

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
get_type(<<"GIF8", X, "a", _/binary>>) when X == $7; X == $9 ->
    gif;
get_type(_) ->
    unknown.

-spec code(img_type()) -> char().
code(png) -> $p;
code(jpeg) -> $j;
code(webp) -> $w;
code(gif) -> $g.

call(Cmd) ->
    case eimp_worker:call(Cmd) of
	{ok, <<ResCode, Reply/binary>>} ->
	    if ResCode == $0 ->
		    {ok, Reply};
	       true ->
		    {error, erlang:binary_to_atom(Reply, latin1)}
	    end;
	{error, _} = Err ->
	    Err
    end.

encode_options(Opts) ->
    {ScaleW, ScaleH} = case lists:keyfind(scale, 1, Opts) of
			   {scale, {W, H}} when W >= 0, H >= 0 ->
			       {W, H};
			   false ->
			       {0, 0};
			   _ ->
			       erlang:error(badarg)
		       end,
    <<ScaleW:16, ScaleH:16>>.

-ifdef(HAVE_GD).
is_gd_compiled() -> true.
-else.
is_gd_compiled() -> false.
-endif.

-ifdef(HAVE_PNG).
is_png_compiled() -> true.
-else.
is_png_compiled() -> false.
-endif.

-ifdef(HAVE_JPEG).
is_jpeg_compiled() -> true.
-else.
is_jpeg_compiled() -> false.
-endif.

-ifdef(HAVE_WEBP).
is_webp_compiled() -> true.
-else.
is_webp_compiled() -> false.
-endif.
