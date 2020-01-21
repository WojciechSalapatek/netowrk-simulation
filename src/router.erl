%%%-------------------------------------------------------------------
%%% @author wojciech
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Jan 2020 11:48
%%%-------------------------------------------------------------------
-module(router).
-define(UPDATE_TIME, 1000).
-author("wojciech").

%% API
-export([create_router/1]).

create_router(Name) ->
  Interfaces = l3_interfaces:create(),
  RTable = routing_table:create(),
  timer:send_interval(?UPDATE_TIME, rip_update),
  router(Name, RTable, Interfaces).

router(Name, RTable, Interfaces) ->
  receive
    info ->
      io:format("[~w]: Info", [Name]),
      l3_interfaces:print_pretty(Interfaces, Name),
      ui:table_endline(),
      routing_table:print_pretty(RTable, Name),
      ui:table_endline(),
      router(Name, RTable, Interfaces);
    {connect, IntName, Ip, Mask, ToRouter} ->
      {NewRTable, NewInterfaces} = router_connection_handler:connect(RTable, Interfaces, Name, {connect, IntName, Ip, Mask, ToRouter}),
      router(Name, NewRTable, NewInterfaces);
    {disconnect_port, IntName} ->
      {NewRTable, NewInterfaces} = router_connection_handler:disconnect(RTable, Interfaces, Name, {disconnect, IntName}),
      router(Name, NewRTable, NewInterfaces);
    {disconnect_router, RName} ->
      {NewRTable, NewInterfaces} = router_connection_handler:disconnect_r(RTable, Interfaces, Name, {connect, RName}),
      router(Name, NewRTable, NewInterfaces);
    {'DOWN', Monitor, process, Pid, _} ->
      {NewRTable, NewInterfaces} = router_connection_handler:neighbour_down(RTable, Interfaces, Name, Monitor, Pid),
      router(Name, NewRTable, NewInterfaces);
    {down, Network, From} ->
      NewRTable = router_connection_handler:down(RTable, Interfaces, Network, From),
      router(Name, NewRTable, Interfaces);
    {create_msg, FromPort, ToIp, Msg, TTL} ->
      {_, Ip, _, _, _} = l3_interfaces:get_for_intname(Interfaces, FromPort),
      self() ! {msg, ToIp, undefined, Msg, Ip, Name, TTL},
      router(Name, RTable, Interfaces);
    {arp_req, Ip, SPid} ->
      case l3_interfaces:get_for_ip(Interfaces, Ip) of
        {_, Ip, _, _, _} ->
          io:format("[~w]: Recieved matching arp request, sending response<~s,~w>~n",[Name, Ip, Name]),
          SPid ! {arp_answ, Ip, Name, self()};
        false -> drop
      end,
      router(Name, RTable, Interfaces);
    {msg, _, _, _, _, _, 0} ->
      io:format("[~w]: Message dropped as time to live ended~n", [Name]),
      router(Name, RTable, Interfaces);
    {msg, ToIp, ToMac, Msg, FromIp, FromMac, TTL} ->
      io:format("[~w]: Recieved message from <~s, ~w> with destination <~s, ~w>~n", [Name, FromIp, FromMac, ToIp, ToMac]),
      case l3_interfaces:get_for_ip(Interfaces, ToIp) of
        {IntName, IntIp, _, _, _} ->
          io:format("[~w]: Successfully recieved message ~s at port <~s, ~w> from <~s, ~w>~n", [Name, Msg, IntIp, IntName, FromIp, FromMac]);
        false ->
          GenIp = ip_utils:generalize_network(ToIp, ip_utils:mask_from_ip(ToIp)),
          case l3_interfaces:get_for_generalized_ip(Interfaces, GenIp) of
            {_, _, _, ToDevicePid, _} ->
              io:format("[~w]: Ip ~s matches current network trying interfaces ~n", [Name, ToIp]),
              ToDevicePid ! {msg, ToIp, ToMac, Msg, FromIp, FromMac, TTL-1},
              router(Name, RTable, Interfaces);
            false ->
              io:format("[~w]: Message's destination ip <~s> not in connected network, routing~n", [Name, FromIp]),
              routing_table:send_msg(RTable, Interfaces, Name, {msg, ToIp, ToMac, Msg, FromIp, FromMac, TTL-1})
          end
      end,
      router(Name, RTable, Interfaces);
    rip_update ->
      l3_interfaces:broadcast(Interfaces, {update_table, Name, RTable}),
      router(Name, RTable, Interfaces);
    {update_table, FromName, HisRTable} ->
      NewRTable = routing_table:update(RTable, Name, Interfaces, FromName, HisRTable),
      router(Name, NewRTable, Interfaces);
    stop ->
      ok
  end.
