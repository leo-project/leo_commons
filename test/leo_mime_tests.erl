%%====================================================================
%%
%% Leo Commons
%%
%% Copyright (c) 2012-2014 Rakuten, Inc.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------
%% Mime Test
%% @doc
%% @end
%%====================================================================
-module(leo_mime_tests).
-author('Yoshiyuki Kanno').

-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% TEST
%%--------------------------------------------------------------------
-ifdef(EUNIT).

guess_mime_test_() ->
    [
     fun guess_/0
    ].

guess_() ->
    <<"application/octet-stream">> = leo_mime:guess_mime(""),
    <<"application/octet-stream">> = leo_mime:guess_mime(<<>>),
    <<"application/octet-stream">> = leo_mime:guess_mime(<<"/path/to/x.unknown">>),
    <<"application/zip">> = leo_mime:guess_mime(<<".zip">>),
    <<"text/html">> = leo_mime:guess_mime(<<"/path/to/x.html">>),
    <<"image/gif">> = leo_mime:guess_mime(<<"images/dir/hoge_hoge.gif">>),
    <<"image/png">> = leo_mime:guess_mime(<<"images/dir/hoge_hoge.png">>),
    <<"image/jpeg">> = leo_mime:guess_mime(<<"images/dir/hoge_hoge.jpeg">>),
    <<"image/gif">> = leo_mime:guess_mime(<<"images/dir/hoge_hoge.GIF">>),
    <<"image/png">> = leo_mime:guess_mime(<<"images/dir/hoge_hoge.PNG">>),
    <<"image/jpeg">> = leo_mime:guess_mime(<<"images/dir/hoge_hoge.JPG">>),
    ok.

-endif.

