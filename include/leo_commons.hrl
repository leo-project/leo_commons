%%======================================================================
%%
%% LeoFS Commons
%%
%% Copyright (c) 2012-2018 Rakuten, Inc.
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
%% Leo Commons -  Constant/Macro/Record
%%
%%======================================================================

%% For leo_mnesia
-define(EXPORT_TYPE_TUPLE, tuple).
-define(EXPORT_TYPE_JSON,  json).
-type(export_type() :: ?EXPORT_TYPE_TUPLE | ?EXPORT_TYPE_JSON).


%% Environment values
-define(ETS_ENV_TABLE, 'leo_env_values').


-define(env_log_dir(ServerType),
        case application:get_env(ServerType, log_dir) of
            {ok, EnvLogDir} -> EnvLogDir;
            _ -> "log"
        end).

-define(env_log_level(ServerType),
        case application:get_env(ServerType, log_level) of
            {ok, EnvLogLevel} -> EnvLogLevel;
            _ -> 0
        end).

-define(env_manager_nodes(ServerType),
        case application:get_env(ServerType, managers) of
            {ok, EnvManagerNode} -> EnvManagerNode;
            _ ->
                %% for test-case.
                {ok, CHostname} = inet:gethostname(),
                NewCHostname = "manager_0@" ++ CHostname,
                [NewCHostname]
        end).

-define(update_env_manager_nodes(ServerType, Managers),
        application:set_env(ServerType, managers, Managers)).

-define(env_queue_dir(ServerType),
        case application:get_env(ServerType, queue_dir) of
            {ok, EnvQueueDir} -> EnvQueueDir;
            _ -> "queue"
        end).


%% Request parameters to be able to communicate between LeoGateway and LeoStorage
-record(request, {
                  %% Common Parameters
                  method :: atom(),                   %% http-verb: [get|post|put|delete|head]
                  addr_id = 0 :: non_neg_integer(),   %% ring-address id
                  key = <<>> :: binary(),             %% object's name
                  data = <<>> :: binary(),            %% blob
                  meta = <<>> :: binary(),            %% custom-metadata (user defined metadata)
                  ksize = 0 :: non_neg_integer(),     %% length of object name
                  dsize = 0 :: non_neg_integer(),     %% length of blob
                  msize = 0 :: non_neg_integer(),     %% length of custom-metadata
                  timestamp = 0 :: non_neg_integer(), %% timestamp
                  csize = 0 :: non_neg_integer(),     %% length of chunked object
                  cnumber = 0 :: non_neg_integer(),   %% total number of chunked objects
                  cindex = 0 :: non_neg_integer(),    %% index of chunked objects
                  checksum = 0 :: non_neg_integer(),  %% object's checksum
                  req_id = 0 :: non_neg_integer(),    %% request ID

                  %% SSEC-related parameters
                  ssec_algorithm = <<>> :: binary(),        %% Encryption algorithm (default: AES256)
                  ssec_key = <<>> :: binary(),              %% 256-bit, base64-encoded encryption key
                  ssec_key_hash = <<>> :: binary(),         %% Base64-encoded 128-bit MD5 digest of the encryption key
                  ssec_algorithm_cp_src = <<>> :: binary(), %% Encryption algorithm (default: AES 256) of copy-source
                  ssec_key_cp_src = <<>> :: binary(),       %% 256-bit, base64-encoded encryption key of copy-source
                  ssec_key_hash_cp_src = <<>> :: binary()   %% Base64-encoded 128-bit MD5 digest of the encryption key of copy-source

                  %% @TODO Erasure Code-related parameters
                 }).
