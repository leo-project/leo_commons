%%====================================================================
%%
%% Leo Commons
%%
%% Copyright (c) 2012
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
%% HTTP Util Test
%% @doc
%% @end
%%====================================================================
-module(leo_http_tests).
-author('Yoshiyuki Kanno').
-vsn('0.9.1').

-include("leo_commons.hrl").
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% TEST
%%--------------------------------------------------------------------
-ifdef(EUNIT).
api_test_() ->
    [
     fun key_inc_path_/0,
     fun key_eq_host1_/0,
     fun key_eq_host2_/0,
     fun key_inc_host1_/0,
     fun get_amz_headers_emp_/0,
     fun get_amz_headers_none_/0,
     fun get_amz_headers_normal1_/0,
     fun get_amz_headers_normal2_/0,
     fun rfc1123_date/0
    ].

key_inc_path_() ->
    Host = ?S3_DEFAULT_ENDPOINT,
    Path = "/bucket/path_to_file.png",
    Ret = leo_http:key(Host, Path),
    "/" ++ Expected = Path,
    ?assertEqual(Expected, Ret).

key_eq_host1_() ->
    Host = "www.leofs.com",
    Path = "/images/path_to_file.png",
    Ret = leo_http:key(Host, Path),
    Expected = Host ++ Path,
    ?assertEqual(Expected, Ret).

key_eq_host2_() ->
    Host = "www.leofs.com",
    Path = "/www.leofs.com/path_to_file.png",
    Ret = leo_http:key(Host, Path),
    "/" ++ Expected = Path,
    ?assertEqual(Expected, Ret).

key_inc_host1_() ->
    Bucket = "bucket",
    Host = Bucket ++ "." ++ ?S3_DEFAULT_ENDPOINT,
    Path = "/path_to_file.png",
    Ret = leo_http:key(Host, Path),
    Expected = Bucket ++ Path,
    ?assertEqual(Expected, Ret).

get_amz_headers_emp_() ->
    T1 = gb_trees:empty(),
    ?assertEqual([], leo_http:get_amz_headers(T1)).

get_amz_headers_none_() ->
    T1 = gb_trees:empty(),
    T2 = gb_trees:enter("Date", "Tue, 27 Mar 2007 21:15:45 +0000", T1),
    T3 = gb_trees:enter("Host", "johnsmith.s3.amazonaws.com", T2),
    T4 = gb_trees:enter("Content-Length", 94328, T3),
    T5 = gb_trees:enter("Content-Type", "image/jpeg", T4),
    ?assertEqual([], leo_http:get_amz_headers(T5)).

get_amz_headers_normal1_() ->
    T1 = gb_trees:empty(),
    T2 = gb_trees:enter("Date", "Tue, 27 Mar 2007 21:15:45 +0000", T1),
    Key = "x-amz-date",
    Val = "Tue, 27 Mar 2007 21:20:26 +0000",
    T3 = gb_trees:enter(Key, Val, T2),
    AmzHeaders = leo_http:get_amz_headers(T3),
    ?assertEqual(1, length(AmzHeaders)),
    ?assertEqual({Key, Val}, hd(AmzHeaders)).

get_amz_headers_normal2_() ->
    T1 = gb_trees:empty(),
    Key = "x-amz-date",
    Val = "Tue, 27 Mar 2007 21:20:26 +0000",
    T2 = gb_trees:enter(Key, Val, T1),
    Key2 = "X-Amz-Meta-ReviewedBy",
    Val2 = "jane@johnsmith.net",
    T3 = gb_trees:enter(Key2, Val2, T2),
    AmzHeaders = leo_http:get_amz_headers(T3),
    ?assertEqual(2, length(AmzHeaders)).

rfc1123_date() ->
    ?assertNotEqual([], leo_http:rfc1123_date(leo_utils:now())).

-endif.
