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
%% @author Krzysztof Rutka <krzysztof.rutka@erlang-solutions.com>
%% @copyright 2012 FlowForwarding.org

-define(INFO(Msg, Args), error_logger:info_msg(Msg, Args)).
-define(WARNING(Msg, Args), error_logger:warning_msg(Msg, Args)).
-define(ERROR(Msg, Args), error_logger:error_msg(Msg, Args)).

-define(CAPABILITY_RE,
        "^urn:[a-z:]+:((netconf:base)|(netconf:capability:[a-z-]+)"
        "|(ns:netconf:[a-z-]+)):[0-9]{1}\\.[0-9]{1}(\\?.+)?$").

-type config_name() :: startup   %% :startup capability
                     | candidate %% :candidate capability
                     | running.

-type xml() :: {config, tuple()}.

-type url() :: {url, string()}. %% :url capability

-type target() :: config_name()
                | url().    %% :url capability

-type source() :: config_name()
                | xml()
                | url().    %% :url capability

-type get_config_source() :: config_name()
                           | url().    %% :url capability

-type filter() :: {subtree, tuple()}
                | {xpath, string()} %% :xpath capability
                | undefined.

-type default_operation() :: merge
                           | replace
                           | none.

-type test_option() :: 'test-then-set' %% :validate capability
                     | set             %% :validate capability
                     | 'test-only'     %% :validate capability
                     | undefined.

-type error_option() :: 'stop-on-error'
                      | 'continue-on-error'
                      | 'rollback-on-error' %% :rollback-on-error capability
                      | undefined.

-type config() :: xml()
                | {url, string()}. %% :url capability

-record(edit_config, {
          target :: target(),
          default_operation = merge :: default_operation(),
          test_option :: test_option(),    %% :validate capability
          error_option = 'stop-on-error' :: error_option(),
          config :: config()
         }).

-record(get_config, {
          source :: get_config_source(),
          filter :: filter()
         }).

-record(copy_config, {
          source :: source(),
          target :: target()
         }).

-record(delete_config, {
          target :: target()
         }).

-record(lock, {
          target :: target()
         }).

-record(unlock, {
          target :: target()
         }).

-record(get, {
          filter :: filter()
         }).

-record(close_session, {}).

-record(kill_session, {
          session_id :: integer()
         }).

-type operation() :: #edit_config{}
                   | #get_config{}
                   | #copy_config{}
                   | #delete_config{}
                   | #lock{}
                   | #unlock{}
                   | #get{}
                   | #close_session{}
                   | #kill_session{}.
                   %% | #commit{}          %% :candidate capability
                   %% | #discard_changes{} %% :candidate capability
                   %% | #cancel_commit{}   %% :confirmed-commit capability
                   %% | #validate{}.       %% :validate capability

-record(rpc, {
          message_id :: string(),
          operation :: operation(),
          attributes = [] :: [{atom(), term()}]
         }).

-type error_type() :: transport
                    | rpc
                    | protocol
                    | application.

-type error_tag() :: 'in-use'
                   | 'invalid-value'
                   | 'too-big'
                   | 'missing-attibute'
                   | 'bad-attibute'
                   | 'unknown-attribute'
                   | 'missing-element'
                   | 'bad-element'
                   | 'unknown-element'
                   | 'unknown-namespace'
                   | 'access-denied'
                   | 'lock-denied'
                   | 'resource-denied'
                   | 'rollback-failed'
                   | 'data-exists'
                   | 'data-missing'
                   | 'operation-not-supported'
                   | 'operation-failed'
                   | 'partial-operation'
                   | 'malformed-message'.

-type error_severity() :: error
                        | warning.

-type error_info() :: 'session-id'
                    | 'bad-attribute'
                    | 'bad-element'
                    | 'ok-element'
                    | 'err-element'
                    | 'noop-element'
                    | 'bad-namespace'.

-record(rpc_error, {
          type :: error_type(),
          tag :: error_tag(),
          severity :: error_severity(),
          info = none :: [{error_info(), term()}]
         }).

-record(ok, {}).

-record(rpc_reply, {
          message_id :: string(),
          content :: #ok{} | #rpc_error{},
          attibutes = [] :: [{atom(), term()}]
         }).

-type rpc() :: #rpc{}
             | #rpc_reply{}.

-type capability_name() :: base
                         | 'writable-running'
                         | candidate
                         | 'rollback-on-error'
                         | startup
                         | url
                         | xpath
                         | 'confirmed-commit'
                         | validate.

-type capability() :: {capability_name(), {integer(), integer()}}.

-record(hello, {
          capabilities = [] :: [capability()]
         }).
