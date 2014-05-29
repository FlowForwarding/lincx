%%
%% See LICENSE
%%

%% @author Cloudozer LLP <info@cloudozer.com>
%% @copyright FlowForwarding.org

-module(linc_max_groups).

-export([modify/2]).
-export([get_desc/2,get_features/1,get_stats/2]).

-include_lib("of_protocol/include/of_protocol.hrl").
-include_lib("of_protocol/include/ofp_v4.hrl").
-include("linc_max.hrl").

modify(_SwitchId, _GroupMod) ->
	%%TODO
	{error,#ofp_error_msg{type =group_mod_failed,
						  code =eperm}}.

get_desc(_SwitchId, _Request) ->
	%%TODO
	#ofp_group_desc_reply{body =[]}.

get_features(_Request) ->
	%%TODO
    #ofp_group_features_reply{
       types = [all, select, indirect, ff],
       capabilities = [select_weight, chaining], %select_liveness, chaining_checks
       max_groups = {?MAX, ?MAX, ?MAX, ?MAX},
       actions = {?SUPPORTED_WRITE_ACTIONS, ?SUPPORTED_WRITE_ACTIONS,
                  ?SUPPORTED_WRITE_ACTIONS, ?SUPPORTED_WRITE_ACTIONS}
      }.

get_stats(_SwitchId, _Request) ->
	%%TODO
	#ofp_group_stats_reply{body =[]}.

%%EOF
