%%------------------------------------------------------------------------------
%% Copyright 2012 FlowForwarding.org
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%-----------------------------------------------------------------------------

%% @author Erlang Solutions Ltd. <openflow@erlang-solutions.com>
%% @copyright 2012 FlowForwarding.org
%% @doc Common header file for all protocol versions.

-define(OFP_HEADER_SIZE, 8).

-define(MOD(Version), case Version of
                          3 -> ofp_v3;
                          4 -> ofp_v4;
                          _ -> unsupported
                      end).

%% Header ----------------------------------------------------------------------

-record(ofp_message, {
          version :: integer(),
          type :: atom(),
          xid = 0 :: integer(),
          body %% ofp_message_body()
         }).
-type ofp_message() :: #ofp_message{}.

%% Hello message ---------------------------------------------------------------

-type ofp_hello_element() :: {versionbitmap, [integer()]}.

-record(ofp_hello, {
          elements = [] :: [ofp_hello_element()]
         }).
-type ofp_hello() :: #ofp_hello{}.

%% Parser ----------------------------------------------------------------------

-record(ofp_parser, {
          version :: integer(),
          module :: atom(),
          stack = <<>> :: binary()
         }).
-type ofp_parser() :: #ofp_parser{}.

%% Client ----------------------------------------------------------------------

-record(controller_status, {
          resource_id        :: string(),
          role               :: master | equal | slave,
          controller_ip      :: string(),
          controller_port    :: integer(),
          local_ip           :: string(),
          local_port         :: integer(),
          protocol           :: atom(),
          connection_state   :: atom(),
          current_version    :: integer(),
          supported_versions :: list(integer())
         }).

-record(async_config, {
          master_equal_packet_in = [no_match, action],
          master_equal_port_status = [add, delete, modify],
          master_equal_flow_removed = [idle_timeout, hard_timeout,
                                       delete, group_delete],
          slave_packet_in = [],
          slave_port_status = [add, delete, modify],
          slave_flow_removed = []
         }).
