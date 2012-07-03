%%======================================================================
%%
%% Leo Commons
%%
%% Copyright (c) 2012 Rakuten, Inc.
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
%% ---------------------------------------------------------------------
%% leo_http  - Utils for Gateway's HTTP logic
%% @doc
%% @end
%%======================================================================
-module(leo_http).

-author('Yoshiyuki Kanno').
-vsn('0.9.0').

-export([key/2, key/3]).

-include("leo_commons.hrl").

%% @doc generate the global uniq key used by internal
%%
-spec(key(string(), string()) ->
             string()).
key(Host, Path) ->
    key(?S3_DEFAULT_ENDPOINT, Host, Path).
-spec(key(string(), string(), string()) ->
             string()).
key(EndPoint, Host, Path) ->
    case string:str(Host, EndPoint) of
        %% Bucket equals Host
        0 ->
            [Top|_] = string:tokens(Path, "/"),
            case string:equal(Host, Top) of
                true  -> "/" ++ Key = Path;
                false -> Key = Host ++ Path
            end,
            Key;
        %% Bucket is included in Path
        1 ->
            "/" ++ Key = Path,
            Key;
        %% Bucket is included in Host
        %% strip .s3.amazonaws.com
        Index ->
            Bucket = string:substr(Host, 1, Index - 2),
            Key = Bucket ++ Path,
            Key
    end.
