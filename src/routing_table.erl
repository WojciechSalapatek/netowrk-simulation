%%%-------------------------------------------------------------------
%%% @author wojciech
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Jan 2020 13:09
%%%-------------------------------------------------------------------
-module(routing_table).
-author("wojciech").

%% API
-export([create/0, add_entry_if_better/8, remove_all_through/2,
  print_pretty/2, send_msg/4, update/5, remove_network_through/4, remove_unreachables/3, get_for_pid/2]).

create() -> [].

add_entry_if_better(RTable, Name, Interfaces, ToIp, ToMask, _, _, Hops) when Hops > 15 ->
  Network = ip_utils:generalize_network(ToIp, ToMask),
  case lists:keyfind(Network, 1, RTable) of
    {Network, M, RN, RP, _} ->
      l3_interfaces:broadcast(Interfaces, {update_table, Name, [{Network, M, RN, RP, 16}]}),
      lists:keydelete(Network, 1, RTable);
    false -> RTable
  end;
add_entry_if_better(RTable, _, _, ToIp, ToMask, ThroughRName, ThroughRPid, Hops) ->
  Network = ip_utils:generalize_network(ToIp, ToMask),
  case lists:keyfind(Network, 1, RTable) of
    {Network, _, _, _, Hops} -> RTable;
    {Network, _, _, _, 0} -> RTable;
    {Network, _, ThroughRName, ThroughRPid, _} ->
      UptdRTable = lists:keydelete(Network, 1, RTable),
      [{Network, ToMask, ThroughRName, ThroughRPid, Hops} | UptdRTable];
    {Network, _, _, _, OldHops} ->
      case OldHops =< Hops of
        true -> RTable;
        false ->
          UptdRTable = lists:keydelete(Network, 1, RTable),
          [{Network, ToMask, ThroughRName, ThroughRPid, Hops} | UptdRTable]
      end;
    false ->
      [{Network, ToMask, ThroughRName, ThroughRPid, Hops} | RTable]
  end.

send_msg(_, _, Name, {_, _, _, 0}) ->
  io:format("[~w]: Message's Time to live ends", [Name]),
  drop;
send_msg(RTable, Interfaces, Name, {_, ToIp, ToMac, Msg, FromIp, FromMac, TTL}) ->
  Network = ip_utils:generalize_network(ToIp, ip_utils:mask_from_ip(ToIp)),
  case lists:keyfind(Network, 1, RTable) of
    {Network, _, RName, Pid, _} ->
      case l3_interfaces:get_for_pid(Interfaces, Pid) of
        {_, _, _, _, _} ->
          io:format("[~w] Found proper routing rule, forwarding to ~w~n", [Name, RName]),
          Pid ! {msg, ToIp, ToMac, Msg, FromIp, FromMac, TTL};
        false ->
          io:format("Route ends as no proper destination found in routing table~n", []),
          drop
      end;
    false ->
      io:format("Route ends as no proper destination found in routing table~n", []),
      drop
  end.

remove_all_through(RTable, Through) ->
  lists:filter(fun({_,_,_,T,_}) -> T /= Through  end, RTable).

remove_network_through(RTable, Interfaces, Network, From) ->
  case lists:keyfind(Network, 1, RTable) of
    {Network, _, From, _, _} ->
      l3_interfaces:broadcast(Interfaces, {down, Network, From}),
      lists:keydelete(Network, 1, RTable);
    _ -> RTable
  end.

remove_unreachables(RTable, FromName, HisTable) ->
  HisHops = lists:filter(fun({_,_,FName,_,_}) -> FromName == FName end, RTable),
  Unrechables = lists:filter(fun({Network,_,_,_,_}) -> lists:keymember(Network, 1, HisTable) == false end, HisHops),
  lists:filter(fun({Network,_,_,_,_}) -> lists:keymember(Network, 1, Unrechables) end, RTable).

update(RTable, _, _, _, []) -> RTable;
update(RTable, Name, Interfaces, FromName, [{Network, ToMask, _, _, Hops} | T]) ->
  case l3_interfaces:get_for_router_name(Interfaces, FromName) of
    {_, _, _, ToRouterPid, _} ->
      NewRTable = add_entry_if_better(RTable, Name, Interfaces, Network, ToMask, FromName, ToRouterPid, Hops+1),
      update(NewRTable, Name, Interfaces, FromName, T);
    false -> RTable
  end.

get_for_pid(RTable, Pid) ->
  lists:keyfind(Pid, 4, RTable).

print_pretty(RTable, RName) ->
  io:format("[~w]: Tablica Routingu ~n", [RName]),
  io:fwrite("|~-16s|~-16s|~-10s|~-15s|~-10s|~n", ["Dest network", "Mask", "Hop name", "Next hop pid", "Hops"]),
  print_entries(RTable).

print_entries([]) -> ok;
print_entries([{ToIp, ToMask, ThroughRName, ThroughRPid, Hops} | T]) ->
  io:fwrite("|~-16s|~-16s|~-10w|~-15w|~-10B|~n", [ToIp, ToMask, ThroughRName, ThroughRPid, Hops]),
  print_entries(T).

