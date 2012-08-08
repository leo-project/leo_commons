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
%% Leo Commons - Error Logger Utils
%% @doc
%% @end
%%======================================================================
-module(leo_error_logger_utils).

-author('Yosuke Hara').

-include_lib("eunit/include/eunit.hrl").

-export([append/5]).

%% @doc Append a log message into the log file
%%
-spec(append(error | warn, string(), string(), integer(), string()) ->
             ok).
append(Type, ModString, FunString, Line, Message) ->
    Fun = case Type of
              error -> error_msg;
              warn  -> warning_msg
          end,

    erlang:apply(error_logger, Fun, ["~p,~p,~p,~p~n", [{module, ModString}, {function, FunString},
                                                       {line, Line}, {body, Message}]]).
