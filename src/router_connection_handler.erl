%%%-------------------------------------------------------------------
%%% @author wojciech
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Jan 2020 11:36
%%%-------------------------------------------------------------------
-module(router_connection_handler).
-author("wojciech").

%% API
-export([connect/4, disconnect/4, disconnect_r/4, neighbour_down/5, down/4]).


connect(RTable, Interfaces, Name, {_, IntName, Ip, Mask, ToRouter}) ->
  ToRouterPid = whereis(ToRouter),
  Monitor = erlang:monitor(process,ToRouterPid),
  NewInterfaces = l3_interfaces:connect(Interfaces, IntName, Ip, Mask, ToRouterPid, Monitor),
  io:format("[~w]: ~w connected at interface <~w, ~s>~n", [Name, ToRouter, IntName, Ip]),
  NewRTable = routing_table:add_entry_if_better(RTable, Name, Interfaces, Ip, Mask, Name, self(), 0),
  {NewRTable, NewInterfaces}.


disconnect(RTable, Interfaces, Name, {_, IntName}) ->
  case l3_interfaces:get_for_intname(Interfaces, IntName) of
    {IntName, _, _, Pid, Monitor} ->
      erlang:demonitor(Monitor),
      NewInterfaces = l3_interfaces:disconnect(Interfaces, IntName),
      io:format("[~w]: Nothing is now connected at interface <~w, >~n", [Name, IntName]),
      NewRTable = routing_table:remove_all_through(RTable, Pid),
      {NewRTable, NewInterfaces};
    false ->
      io:format("[~w]: Nothing connected at interface ~w~n", [Name, IntName]),
      {RTable, Interfaces}
  end.


disconnect_r(RTable, Interfaces, Name, {_, RName}) ->
  case l3_interfaces:get_for_router_name(Interfaces, RName) of
    {IntName, _, _, RPid, Monitor} ->
      erlang:demonitor(Monitor),
      NewInterfaces = l3_interfaces:disconnect(Interfaces, IntName),
      io:format("[~w]: ~w is no longer connected~n", [Name, RName]),
      NewRTable = routing_table:remove_all_through(RTable, RPid),
      {NewRTable, NewInterfaces};
    false ->
      io:format("[~w]: ~w is not connected~n", [Name, RName]),
      {RTable, Interfaces}
  end.


neighbour_down(RTable, Interfaces, Name, Monitor, Pid) ->
  {IntName, Network, _, _, Monitor} = l3_interfaces:get_for_monitor(Interfaces, Monitor),
  io:format("[~w]: Down status recived at port ~w, disconnecting ~n", [Name, IntName]),
  NewInterfaces = l3_interfaces:disconnect(Interfaces, IntName),
  NewRTable = routing_table:remove_all_through(RTable, Pid),
  l3_interfaces:broadcast(Interfaces, {down, Network, Name}),
  {NewRTable, NewInterfaces}.


down(RTable, Interfaces, Network, From) ->
  routing_table:remove_network_through(RTable, Interfaces, Network, From).

