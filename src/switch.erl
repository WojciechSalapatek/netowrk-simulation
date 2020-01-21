%%%-------------------------------------------------------------------
%%% @author wojciech
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Jan 2020 18:29
%%%-------------------------------------------------------------------
-module(switch).
-author("wojciech").

%% API
-export([create_switch/1]).

create_switch(Name) ->
  Interfaces = l2_interfaces:create(),
  ArpTable = arp_table:create(),
  MacTable = mac_table:create(),
  switch(Name, ArpTable, MacTable, Interfaces, "0.0.0.0").

switch(Name, ArpTable, MacTable, Interfaces, Gateway) ->
  receive
    info ->
      io:format("[~w]: Info", [Name]),
      l2_interfaces:print_pretty(Interfaces, Name),
      ui:table_endline(),
      arp_table:print_pretty(ArpTable, Name),
      ui:table_endline(),
      mac_table:print_pretty(MacTable, Name),
      ui:table_endline(),
      switch(Name, ArpTable, MacTable, Interfaces, Gateway);
    {set_gateway, GatewayIp} ->
      l2_interfaces:broadcast(Interfaces, {arp_req, GatewayIp}),
      switch(Name, ArpTable, MacTable, Interfaces, GatewayIp);
    {msg, _, _, _, _, _, 0} ->
      switch(Name, ArpTable, MacTable, Interfaces, Gateway);
    {msg, ToIp, undefined, Msg, FromIp, FromMac, TTL} ->
      CurrNetwork = ip_utils:generalize_network(Gateway, ip_utils:mask_from_ip(Gateway)),
      DestNetwork = ip_utils:generalize_network(ToIp, ip_utils:mask_from_ip(ToIp)),
      case  CurrNetwork == DestNetwork of
        true ->
          io:format("[~w]: Destination Ip <~s> in local network <~s>~n", [Name, ToIp, CurrNetwork]),
          case arp_table:get_for_ip(ArpTable, ToIp) of
            {ToIp, Mac} ->
              case mac_table:get_for_mac(MacTable, Mac) of
                {Port, Mac} ->
                  io:format("[~w]: Found destination <~s, ~w, ~w> sending message~n", [Name, ToIp, Mac, Port]),
                  l2_interfaces:send(Interfaces, Port, {msg, ToIp, Mac, Msg, FromIp, FromMac, TTL-1});
                false ->
                  io:format("[~w]: Destination <~s, ~w> not in mac table, sending mac req~n", [Name, ToIp, Mac]),
                  l2_interfaces:broadcast(Interfaces, {mac_req, Mac, self()}),
                  self() ! {msg, ToIp, Mac, Msg, FromIp, FromMac, TTL-1}
              end;
            false ->
              io:format("[~w]: Destination Ip <~s> not in arp table, sending arp req~n", [Name, ToIp]),
              l2_interfaces:broadcast(Interfaces, {arp_req, ToIp, self()}),
              self() ! {msg, ToIp, undefined, Msg, FromIp, FromMac, TTL-1}
          end;
        false ->
          io:format("[~w]: Destination Ip <~s> not in local network, sending to gateway ~s~n", [Name, ToIp, Gateway]),
          case arp_table:get_for_ip(ArpTable, Gateway) of
            {Gateway, Mac} ->
              case mac_table:get_for_mac(MacTable, Mac) of
                {Port, Mac} ->
                  io:format("[~w]: Found destination <~s, ~w, ~w> sending message~n", [Name, ToIp, Mac, Port]),
                  l2_interfaces:send(Interfaces, Port, {msg, ToIp, undefined, Msg, FromIp, FromMac, TTL-1});
                false ->
                  io:format("[~w]: Destination <~s, ~w> not in mac table, sending mac req~n", [Name, Gateway, Mac]),
                  l2_interfaces:broadcast(Interfaces, {mac_req, Mac, self()}),
                  self() ! {msg, ToIp, Mac, Msg, FromIp, FromMac, TTL-1}
              end;
            false ->
              io:format("[~w]: Destination Ip <~s> not in arp table, sending arp req~n", [Name, Gateway]),
              l2_interfaces:broadcast(Interfaces, {arp_req, Gateway, self()}),
              self() ! {msg, ToIp, undefined, Msg, FromIp, FromMac, TTL-1}
          end
      end,
      switch(Name, ArpTable, MacTable, Interfaces, Gateway);
    {msg, ToIp, ToMac, Msg, FromIp, FromMac, TTL} ->
      case mac_table:get_for_mac(MacTable, ToMac) of
        {Port, ToMac} ->
          io:format("[~w]: Found destination <~s, ~w, ~w> sending message~n", [Name, ToIp, ToMac, Port]),
          l2_interfaces:send(Interfaces, Port, {msg, ToIp, ToMac, Msg, FromIp, FromMac, TTL-1});
        false ->
          io:format("[~w]: Destination <~s, ~w> not in mac table, sending mac req~n", [Name, ToIp, ToMac]),
          l2_interfaces:broadcast(Interfaces, {mac_req, ToMac, self()}),
          self() ! {msg, ToIp, ToMac, Msg, FromIp, FromMac, TTL-1}
      end,
      switch(Name, ArpTable, MacTable, Interfaces, Gateway);
    {mac_answ, Mac, DevPid} ->
      {Port, DevPid, _} = l2_interfaces:get_for_pid(Interfaces, DevPid),
      NewMacs = mac_table:add(MacTable, Port, Mac),
      switch(Name, ArpTable, NewMacs, Interfaces, Gateway);
    {arp_answ, Ip, Mac, DevPid} ->
      {Port, DevPid, _} = l2_interfaces:get_for_pid(Interfaces, DevPid),
      NewArps = arp_table:add(ArpTable, Ip, Mac),
      NewMacs = mac_table:add(MacTable, Port, Mac),
      switch(Name, NewArps, NewMacs, Interfaces, Gateway);
    {connect, Port, DeviceName} ->
      Pid = whereis(DeviceName),
      Monitor = erlang:monitor(process, Pid),
      NewInterfaces = l2_interfaces:connect(Interfaces, Port, Pid, Monitor),
      io:format("[~w]: ~w connected at interface ~w~n", [Name, DeviceName, Port]),
      switch(Name, ArpTable, MacTable, NewInterfaces, Gateway);
    stop ->
      ok
  end.
