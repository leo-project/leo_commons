%%======================================================================
%%
%% Leo Commons
%%
%% Copyright (c) 2012 Rakuten, Inc
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
%% Leo Commons - hashtable
%% @doc
%% @end
%%======================================================================
-module(leo_hashtable).


-author('Yosuke Hara').
-vsn('0.9.1').

-include_lib("eunit/include/eunit.hrl").

-export([new/0, new/1,
         destroy/1,
         get/2, get/3,
         put/3,
         append/3,
         clear/1,
         delete/2,
         all/1,
         keys/1,
         incr/2]).
-export([loop/1]).

%%----------------------------------------------------------------------
%% APIs
%%----------------------------------------------------------------------
%% @doc
%%
-spec(new() -> pid() | {error, any()}).
new() ->
    spawn(?MODULE, loop, [mutable]).

-spec(new(immutable) -> pid() | {error, any()}).
new(immutable) ->
    spawn(?MODULE, loop, [immutable]).

%% @doc
%%
-spec(destroy(Hashtble::pid()) -> ok).
destroy(Hashtable) ->
    exit(Hashtable, destroy).

%% @doc
%%
-spec(get(Hashtable::pid(), K::any()) -> any()).
get(Hashtable, K) ->
    Hashtable ! {self(), get, K},
    receive
        Res ->
            Res
    end.

-spec(get(Hashtable::pid(), K::any(), UndefinedValue::any()) -> any()).
get(Hashtable, K, UndefinedValue) ->
    case get(Hashtable, K) of
        undefined -> UndefinedValue;
        Value -> Value
    end.

%% @doc
%%
-spec(put(Hashtable::pid(), K::any(), V::any()) -> ok).
put(Hashtable, K, V) ->
    Hashtable ! {self(), put, K, V},
    receive
        Res ->
            Res
    end.

%% @doc
%%
-spec(append(Hashtable::pid(), K::any(), V::any()) -> ok | {error, any()}).
append(Hashtable, K, V) ->
    case catch put(Hashtable, K,
                   get(Hashtable, K, []) ++ [V]) of
        ok ->
            ok;
        _ ->
            {error, could_not_store}
    end.

%% @doc
%%
-spec(clear(Hashtable::pid()) -> ok).
clear(Hashtable) ->
    case all(Hashtable) of
        [] ->
            ok;
        List ->
            lists:foreach(fun({K,_}) ->
                                  delete(Hashtable, K)
                          end, List)
    end.

%% @doc
%%
-spec(delete(Hashtable::pid(), K::any()) -> ok).
delete(Hashtable, K) ->
    Hashtable ! {self(), delete, K},
    receive
        Res ->
            Res
    end.

%% @doc
%%
-spec(all(Hashtable::pid()) -> list()).
all(Hashtable) ->
    Hashtable ! {self(), all},
    receive
        Res ->
            lists:reverse(Res)
    end.

%% @doc
%%
-spec(keys(Hashtable::pid()) -> list()).
keys(Hashtable) ->
    lists:map(fun({X,_}) ->
                      X
              end, all(Hashtable)).

%% @doc
%%
-spec(incr(Hashtable::pid(), K::any()) -> any()).
incr(Hashtable, K) ->
    put(Hashtable, K,
        get(Hashtable, K, 0) + 1).


%%----------------------------------------------------------------------
%% INTERNAL-FUNCTION(S)
%%----------------------------------------------------------------------
loop(Type) ->
    receive
        {Pid, get, K} ->
            Pid ! get(K);
        {Pid, put, K, V} when Type == mutable ->
            put(K, V),
            Pid ! ok;
        {Pid, put, K, V} when Type == immutable ->
            case get(K) of
                undefined ->
                    put(K, V),
                    Pid ! ok;
                _ ->
                    Pid ! {error, already_defined}
            end;
        {Pid, delete, K} ->
            Pid ! erase(K);
        {Pid, all} ->
            Pid ! get()            
    end,
    loop(Type).

%%----------------------------------------------------------------------
%% TEST
%%----------------------------------------------------------------------

