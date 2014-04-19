%%
%%
%%

%% @author Cloudozer LLP. <info@cloudozer.com> 
%% @copyright 2012 FlowForwarding.org
-module(linc_max_port_native).
-export([vif/1]).

-define(VIF_MAILBOX_LIMIT, 131072).

-spec vif(string()) -> {ok, port(), binary()} |
					   {error, term()}.
vif(Interface) ->
	case net_vif:open(Interface, [binary,{mailbox_limit,?VIF_MAILBOX_LIMIT}]) of
	{ok,Port} ->
		HwAddr = get_hw_addr(Interface),
		{ok,Port,HwAddr};

	{error,_} =Error ->
		Error
	end.

-spec get_hw_addr(string()) -> binary().
get_hw_addr(Interface) ->
    {ok,Ifs} = inet:getifaddrs(),
    DefaultMAC = <<0,0,0,0,0,0>>,
    case lists:keyfind(Interface, 1, Ifs) of
        false ->
            DefaultMAC;
        {Interface, Opts} ->
            case lists:keyfind(hwaddr, 1, Opts) of
                false ->
                    DefaultMAC;
                {hwaddr, MAC} ->
                    list_to_binary(MAC)
            end
    end.

%%EOF
