%%
%%
%%

%% @author Cloduozer LLP. <info@cloudozer.com>
%% @copyright 2012 FlowForwarding.org
-module(linc_max_fast_actions).
-export([update_metadata/3]).
-export([meter/2]).
-export([apply_set/3]).
-export([apply_list/3]).

-include_lib("of_protocol/include/of_protocol.hrl").
-include_lib("of_protocol/include/ofp_v4.hrl").
-include_lib("linc/include/linc_logger.hrl").
-include_lib("pkt/include/pkt.hrl").
-include("linc_max.hrl").

-include("fast_path.hrl").

%% FAST PATH
%%
%% The function is needed because metadata are represented as binary in the
%% argument list of a flow entry.
%%
update_metadata(<<MetaInt:64>>, AndMe, OrMe) ->
	<<(MetaInt band AndMe bor OrMe):64>>.

%% FAST PATH
%%
%% Meters are processes. The call should lookup the meter process using the
%% state and exchange messages with it to check that the packet fits the bands.
%%
meter(_MeterId, _St) -> ok.

%% Action Set

%% FAST PATH
%%
apply_set(#fast_actions{output =PortNo}, Frame, Blaze) when is_integer(PortNo) ->
	{_,Outlet,_} = lists:keyfind(PortNo, 1, Blaze#blaze.ports),
	erlang:port_command(Outlet, Frame);

apply_set(#fast_actions{output =controller}, Frame, _Blaze) ->

	%% 
	%% This is a quick-n-dirty implementation for Packet-In messages. It follows
	%% the code of linc_us4 without much thought. Require a good review.
	%%

	SwitchId = 0,	%%XXX
	TableId = 0,	%%XXX
	PacketIn = #ofp_packet_in{reason = action,
							  table_id = TableId,
							  data = Frame},
	?INFO("Packet-In [1]: ~p\n", [pkt:decapsulate(Frame)]),
    linc_logic:send_to_controllers(SwitchId, #ofp_message{body = PacketIn});

apply_set(#fast_actions{}, _Frame, _Blaze) ->
	drop;	%% empty action set

apply_set(Actions, _Frame, _Blaze) ->
	io:format("? ~p\n", [Actions]).

%% Apply-Actions

%% FAST PATH
%%
apply_list([], Frame, _Blaze) ->
	Frame;

apply_list([{output,PortNo}|ActionList], Frame, Blaze) when is_integer(PortNo) ->
	{_,Outlet,_} = lists:keyfind(PortNo, 1, Blaze#blaze.ports),
	erlang:port_command(Outlet, Frame),
	apply_list(ActionList, Frame, Blaze);
apply_list([{output,controller}|ActionList], Frame, Blaze) ->
	%% See comment above
	SwitchId = 0,	%%XXX
	TableId = 0,	%%XXX
	PacketIn = #ofp_packet_in{reason = action,
							  table_id = TableId,
							  data = Frame},
	?INFO("Packet-In [2]: ~p\n", [pkt:decapsulate(Frame)]),
    linc_logic:send_to_controllers(SwitchId, #ofp_message{body = PacketIn}),
	apply_list(ActionList, Frame, Blaze);

apply_list([{set_queue,_Queue}|ActionList], Frame, Blaze) ->
	?ERROR("Queues not supported"),
	apply_list(ActionList, Frame, Blaze);

apply_list([{group,_Group}|ActionList], Frame, Blaze) ->
	?ERROR("Groups not supported"),
	apply_list(ActionList, Frame, Blaze);

apply_list([{push_vlan,EthType}|ActionList], Frame, Blaze) ->
	Frame1 = push_vlan(Frame, EthType),
	apply_list(ActionList, Frame1, Blaze);
apply_list([pop_vlan|ActionList], Frame, Blaze) ->
	Frame1 = pop_vlan(Frame),
	apply_list(ActionList, Frame1, Blaze);

apply_list([{push_mpls,EthType}|ActionList], Frame, Blaze) ->
	Frame1 = push_mpls(Frame, EthType),
	apply_list(ActionList, Frame1, Blaze);
apply_list([{pop_mpls,EthType}|ActionList], Frame, Blaze) ->
	Frame1 = pop_mpls(Frame, EthType),
	apply_list(ActionList, Frame1, Blaze);

apply_list([{push_pbb,EthType}|ActionList], Frame, Blaze) ->
	Frame1 = push_pbb(Frame, EthType),
	apply_list(ActionList, Frame1, Blaze);
apply_list([pop_pbb|ActionList], Frame, Blaze) ->
	Frame1 = pop_pbb(Frame),
	apply_list(ActionList, Frame1, Blaze);

apply_list([{set_field,Field,Value}|ActionList], Frame, Blaze) ->
	case set_field(Frame, Field, Value) of
	Error when is_atom(Error) ->
		Error;	%% missing, protected, fragmented
	Frame1 ->
		apply_list(ActionList, Frame1, Blaze)
	end;

apply_list([{set_mpls_ttl,TTL}|ActionList], Frame, Blaze) ->
	Frame1  = set_mpls_ttl(Frame, TTL),
	apply_list(ActionList, Frame1, Blaze);
apply_list([decrement_mpls_ttl|ActionList], Frame, Blaze) ->
	case decrement_mpls_ttl(Frame) of
	invalid_ttl ->
		?INFO("Packet has invalid TTL"),
		%%TODO: send packet-in message to controller
		invalid_ttl;
	Frame1 ->
		apply_list(ActionList, Frame1, Blaze)
	end;

apply_list([{set_ip_ttl,TTL}|ActionList], Frame, Blaze) ->
	Frame1  = set_ip_ttl(Frame, TTL),
	apply_list(ActionList, Frame1, Blaze);
apply_list([decrement_ip_ttl|ActionList], Frame, Blaze) ->
	case decrement_ip_ttl(Frame) of
	invalid_ttl ->
		?INFO("Packet has invalid TTL"),
		%%TODO: send packet-in message to controller
		invalid_ttl;
	Frame1 ->
		apply_list(ActionList, Frame1, Blaze)
	end;

apply_list([copy_ttl_outwards|ActionList], Frame, Blaze) ->
	Frame1  = copy_ttl_outwards(Frame),
	apply_list(ActionList, Frame1, Blaze);
apply_list([copy_ttl_inwards|ActionList], Frame, Blaze) ->
	Frame1  = copy_ttl_inwards(Frame),
	apply_list(ActionList, Frame1, Blaze).

set_field(Frame, Field, FastValue) ->
	linc_max_splicer:edit(Frame, Field, FastValue).

%%------------------------------------------------------------------------------
%% These are slow - the faster version should not use pkt:*
%%
%% Copied from linc_max_actions.erl
%%

push_vlan(Frame, EthType) ->
	P = pkt:decapsulate(Frame),
    %% When pushing, fields are based on existing tag if there is any
    case linc_max_packet:find(P, ieee802_1q_tag) of
        not_found ->
            InheritVid = <<1:12>>,
            InheritPrio = 0;
        {_, BasedOnTag} ->
            InheritVid = BasedOnTag#ieee802_1q_tag.vid,
            InheritPrio = BasedOnTag#ieee802_1q_tag.pcp
    end,
    P2 = linc_max_packet:find_and_edit(
           P, ether,
           fun(T) -> 
                   NewTag = #ieee802_1q_tag{
                     pcp = InheritPrio,
                     vid = InheritVid,
                     ether_type = EthType
                    },
                   %% found ether element, return it plus VLAN tag for insertion
                   [T, NewTag]
           end),
	pkt:encapsulate(P2).

pop_vlan(Frame) ->
	P = pkt:decapsulate(Frame),
    P2 = linc_max_packet:find_and_edit(
           P, ieee802_1q_tag,
           %% returning 'delete' atom will work for first VLAN tag only
           fun(_) -> 'delete' end),
	pkt:encapsulate(P2).

%%XXX: EthType is not used
push_mpls(Frame, _EthType) ->
	P = pkt:decapsulate(Frame),
	%% inherit IP or MPLS ttl value
    FindOldMPLS = linc_max_packet:find(P, mpls_tag),
    SetTTL = case linc_max_packet:find(P, ipv4) of
                 not_found ->
                     case FindOldMPLS of
                         not_found ->
                             0;
                         {_, T} ->
                             mpls_get_outermost_ttl(T)
                     end;
                 {_, T} ->
                     T#ipv4.ttl
             end,

    case FindOldMPLS of
        not_found ->
            %% Must insert after ether or vlan tag,
            %% whichever is deeper in the packet
            InsertAfter = case linc_max_packet:find(P, ieee802_1q_tag) of
                              not_found ->
                                  ether;
                              _ ->
                                  ieee802_1q_tag
                          end,
            P2 = linc_max_packet:find_and_edit(
                   P, InsertAfter,
                   fun(T) -> 
                           NewEntry = #mpls_stack_entry{ttl = SetTTL},
                           NewTag = #mpls_tag{stack = [NewEntry]},
                           %% found ether or vlan element, return it plus
                           %% MPLS tag for insertion
                           [T, NewTag]
                   end);
        %% found an MPLS shim header, and will push tag into it
        _ ->
            P2 = linc_max_packet:find_and_edit(
                   P, mpls_tag,
                   fun(T) -> 
                           %% base the newly inserted entry on a previous one
                           NewEntry = hd(T#mpls_tag.stack),
                           T#mpls_tag{stack = [NewEntry | T#mpls_tag.stack]}
                   end)
    end,
	pkt:encapsulate(P2).

pop_mpls(Frame, EthType) ->
	P = pkt:decapsulate(Frame),
    PopMPLSHeader = fun(T) ->
                            Stk = T#mpls_tag.stack,
                            %% based on how many elements were in stack,
                            %% either pop a top most element or delete
                            %% the whole tag (for empty)
                            case Stk of
                                [_OnlyOneElement] ->
                                    'delete';
                                [_|RestOfStack] ->
                                    T#mpls_tag{stack = RestOfStack}
                            end
                    end,
    ModifyEtherType = fun(T) ->
                              case T of
                                  #ether{} ->
                                      T#ether{type = EthType};
                                  #ieee802_1q_tag{} ->
                                      T#ieee802_1q_tag{ether_type = EthType}
                              end
                      end,
    P2 = case linc_max_packet:find_and_edit(P, mpls_tag, PopMPLSHeader) of
             Unmodified when Unmodified =:= P ->
                 Unmodified;
             Modified ->
                 BeforeMPLSTag = case linc_max_packet:find(P, ieee802_1q_tag) of
                                     not_found ->
                                         ether;
                                     _ ->
                                         ieee802_1q_tag
                                 end,
                 linc_max_packet:find_and_edit(Modified, BeforeMPLSTag,
                                               ModifyEtherType)
         end,
	pkt:encapsulate(P2).

push_pbb(Frame, 16#88e7) ->
	%%
	%% pkt module has separate entries for ether and pbb packets. Try pbb first.
	%%
	P =  try
		pkt:decapsulate_pbb(Frame)
	catch _:_ ->
		pkt:decapsulate(Frame)
	end,

    %% If there was PBB tag, copy isid from it
    {ISID, IsPreviousPBB} = case linc_max_packet:find(P, pbb) of
                                not_found ->
                                    {<<1:24>>, false};
                                {_, PreviousPBB} ->
                                    {PreviousPBB#pbb.i_sid, true}
                            end,
    %% If there was VLAN tag, copy PCP from it
    IPCP = case linc_max_packet:find(P, ieee802_1q_tag) of
               not_found ->
                   0;
               {_, PreviousVLAN} ->
                   PreviousVLAN#ieee802_1q_tag.pcp
           end,
    PBB = #pbb{b_pcp = 0,
               b_dei = 0,
               i_pcp = IPCP,
               i_dei = 0,
               i_uca = 0,
               i_sid = ISID},
    NewPacket = case IsPreviousPBB of
                    true ->
                        [#pbb{} | PacketRest] = P,
                        [PBB | PacketRest];
                    false ->
                        [PBB | P]
                end,
	pkt:encapsulate(NewPacket).

pop_pbb(Frame) ->
	%% see comment above
	P =  try
		pkt:decapsulate_pbb(Frame)
	catch _:_ ->
		pkt:decapsulate(Frame)
	end,
    P2 = case P of
             [#pbb{} | PRest] ->
                 PRest;
             _ ->
                 P
         end,
	pkt:encapsulate(P2).

set_mpls_ttl(Frame, NewTTL) ->
	P = pkt:decapsulate(Frame),
    P2 = linc_max_packet:find_and_edit(
           P, mpls_tag,
           fun(T) ->
                   [TopTag | StackTail] = T#mpls_tag.stack,
                   NewTag = TopTag#mpls_stack_entry{ ttl = NewTTL },
                   T#mpls_tag{ stack = [NewTag | StackTail] }
           end),
	pkt:encapsulate(P2).

decrement_mpls_ttl(Frame) ->
	P = pkt:decapsulate(Frame),
    try 
        P2 = linc_max_packet:find_and_edit(
               P, mpls_tag,
               fun(#mpls_tag{stack=[#mpls_stack_entry{ttl=0} | _]}) ->
                       throw(invalid_ttl);
                  (#mpls_tag{stack=[#mpls_stack_entry{ttl=TTL}=TopTag | StackTail]}=T) ->
                       NewTag = TopTag#mpls_stack_entry{ttl = TTL-1},
                       T#mpls_tag{ stack = [NewTag | StackTail] }
               end),
		pkt:encapsulate(P2)
    catch 
        throw:invalid_ttl =Error ->
		Error
	end.

set_ip_ttl(Frame, NewTTL) ->
	P = pkt:decapsulate(Frame),
    P2 = linc_max_packet:find_and_edit(
           P, ipv4,
           fun(T) ->
                   T#ipv4{ ttl = NewTTL }
           end),
	pkt:encapsulate(P2).

decrement_ip_ttl(Frame) ->
	P = pkt:decapsulate(Frame),
    try
        P2 = linc_max_packet:find_and_edit(
               P, ipv4,
               fun(#ipv4{ ttl = 0 }) ->
                       throw(invalid_ttl);
                  (#ipv4{ ttl = TTL } = T) ->
                       T#ipv4{ ttl = TTL-1 }
               end),
		pkt:encapsulate(P2)
    catch
        throw:invalid_ttl = Error ->
		Error
	end.

copy_ttl_outwards(Frame) ->
	P = pkt:decapsulate(Frame),
    Tags = filter_copy_fields(P),
    P2 = case Tags of
             [#mpls_tag{stack = S}, #ipv4{ttl = NextOutermostTTL} | _]
               when length(S) == 1 ->
                 linc_max_packet:find_and_edit(
                   P, mpls_tag,
                   fun(T) ->
                           [Stack1 | StackRest] = T#mpls_tag.stack,
                           Stack1b = Stack1#mpls_stack_entry{
                                       ttl = NextOutermostTTL
                                      },
                           T#mpls_tag{
                             stack = [Stack1b | StackRest]
                            }
                   end);
             [#mpls_tag{stack = S} | _] when length(S) > 1 ->
                 linc_max_packet:find_and_edit(
                   P, mpls_tag,
                   fun(T) ->
                           [Stack1, Stack2 | StackRest] = T#mpls_tag.stack,
                           Stack1b = Stack1#mpls_stack_entry{
                                       ttl = Stack2#mpls_stack_entry.ttl
                                      },
                           T#mpls_tag{
                             %% reconstruct the stack
                             stack = [Stack1b, Stack2 | StackRest] 
                            }
                   end);
             [#ipv4{}, #ipv4{ttl = NextOutermostTTL}] ->
                 linc_max_packet:find_and_edit(
                   P, ipv4,
                   fun(T) ->
                           T#ipv4{ ttl = NextOutermostTTL }
                   end)
         end,
	pkt:encapsulate(P2).

copy_ttl_inwards(Frame) ->
	P = pkt:decapsulate(Frame),
	Tags = filter_copy_fields(P),
    P2 = case Tags of
             [#mpls_tag{stack = S} = MPLS, #ipv4{} | _]
               when length(S) == 1 ->
                 linc_max_packet:find_and_edit(
                   P, ipv4,
                   fun(T) ->
                           T#ipv4{ ttl = mpls_get_outermost_ttl(MPLS) }
                   end);
             [#mpls_tag{stack = S} | _] when length(S) > 1 ->
                 linc_max_packet:find_and_edit(
                   P, mpls_tag,
                   fun(T) ->
                           [Stack1, Stack2 | StackRest] = T#mpls_tag.stack,
                           Stack2b = Stack2#mpls_stack_entry{
                                       ttl = Stack1#mpls_stack_entry.ttl
                                      },
                           T#mpls_tag{
                             stack = [Stack1, Stack2b | StackRest]
                            }
                   end);
             [#ipv4{ttl = OutermostTTL}, #ipv4{}] ->
                 linc_max_packet:find_and_edit_skip(
                   P, ipv4,
                   fun(T) ->
                           T#ipv4{ ttl = OutermostTTL }
                   end, 1)
         end,
	pkt:encapsulate(P2).
 
%% @doc Extracts a TTL value from given MPLS tag's stack topmost entry
mpls_get_outermost_ttl(T = #mpls_tag{}) ->
    [H | _] = T#mpls_tag.stack,
    H#mpls_stack_entry.ttl.

filter_copy_fields(List) ->
    lists:filter(fun(B) when is_binary(B) ->
					false;
				 (T) when is_tuple(T) ->
                         element(1,T) =:= mpls_tag orelse
                             element(1,T) =:= ipv4
                 end, List).

%%EOF
