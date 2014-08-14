%%
%% See LICENSE
%%

%% @author Cloudozer LLP <info@cloudozer.com>
%% @copyright FlowForwarding.org

-module(linc_max_groups).

-export([id_to_name/1]).
-export([modify/2]).
-export([get_desc/2,get_features/1,get_stats/2,is_valid/2,update_reference_count/3]).

-include_lib("of_protocol/include/of_protocol.hrl").
-include_lib("of_protocol/include/ofp_v4.hrl").
-include("linc_max.hrl").

id_to_name(GroupId) ->
	list_to_atom("group_" ++ integer_to_list(GroupId)).

modify(_SwitchId, #ofp_group_mod{
	command = delete,
	group_id = _GroupId}
) ->
	%{error,#ofp_error_msg{type =group_mod_failed,code =eperm}};
	ok;
modify(_SwitchId, #ofp_group_mod{
	command = _Command,
	type = Type,
	group_id = GroupId,
	buckets = Buckets}
) ->
	Name = id_to_name(GroupId),

	Forms = [
		{attribute,0,module,Name},
		{attribute,0,export,[{type,0},{apply,2}]},
		{function,0,type,0,[
			{clause,0,[],[],[{atom,0,Type}]}
		]},
		{function,0,apply,2,[
			{clause,0,[{var,0,'Packet'},{var,0,'Blaze'}],[],body(Type, Buckets)}
		]}
	],

	{ok,Name,Bin} = compile:forms(Forms, [report_errors]),
	case erlang:check_old_code(Name) of
	true ->
		erlang:purge_module(Name);
	_ ->
		ok
	end,
	{module,_} = code:load_binary(Name, "generated", Bin),
	ok.

body(all, Buckets) ->
	lists:map(
		fun(#ofp_bucket{actions = Actions}) ->
			{ActionList, _} = linc_max_generator:action_list(Actions),
			ActionList
		end,
		Buckets
	);
body(select, Buckets) ->
	{CaseBody, TotalWeight} = lists:mapfoldl(
		fun(#ofp_bucket{actions = Actions, weight = Weight}, TotalWeight) ->
			{ActionList, _} = linc_max_generator:action_list(Actions),
			NewTotalWeight = TotalWeight + Weight,
			{
				{clause,0,
					[{var,0,'R'}],
					[[{op,0,'=<',{var,0,'R'},{integer,0,NewTotalWeight}}]],
					[ActionList]
				},
				NewTotalWeight
			}
		end,
		0,
		Buckets
	),

	[{'case',0,
		{call,0,{remote,0,{atom,0,random},{atom,0,uniform}},[{integer,0,TotalWeight}]},
		CaseBody
	}].

get_desc(_SwitchId, _Request) ->
	Stats = lists:foldl(
		fun({Mod, _}, Stats) ->
			case string:tokens(atom_to_list(Mod), "_") of
				["group", Id] ->
					Type = Mod:type(),
					GroupId = list_to_integer(Id),
					[#ofp_group_desc_stats{type = Type, group_id = GroupId} | Stats];
				_ ->
					Stats
			end
		end,
		[],
		code:all_loaded()
	),
	#ofp_group_desc_reply{body = Stats}.

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

is_valid(_SwitchId, _GroupId) ->
	true.

update_reference_count(_SwitchId, _Group, _Incr) ->
	ok.
%%EOF
