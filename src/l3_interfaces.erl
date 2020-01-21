%%%-------------------------------------------------------------------
%%% @author wojciech
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Jan 2020 11:57
%%%-------------------------------------------------------------------
-module(l3_interfaces).
-author("wojciech").

%% API
-export([create/0, connect/6, disconnect/2, get_for_intname/2, get_for_router_name/2,
  get_for_monitor/2, print_pretty/2, get_for_ip/2, get_for_pid/2, broadcast/2, get_for_generalized_ip/2]).

create() ->
  [].

connect(Interfaces, IntName, Ip, Mask, ToDevicePid, Monitor) ->
  case ip_utils:correctv4(Ip) of
    match ->
      case lists:keyfind(IntName,1,Interfaces) of
        {IntName, _, _, _, OldMonitor} ->
          erlang:demonitor(OldMonitor),
          Ints = lists:keydelete(IntName, 1, Interfaces),
          [{IntName, Ip, Mask, ToDevicePid, Monitor} | Ints];
        false ->
          [{IntName, Ip, Mask, ToDevicePid, Monitor} | Interfaces]
      end;
    nomatch -> io:format("[Interface~w]: Incorrect ipv4: ~w~n", [IntName, Ip])
  end.

disconnect(Interfaces, IntName) ->
  lists:keydelete(IntName, 1, Interfaces).

get_for_intname(Interfaces, IntName) ->
  lists:keyfind(IntName,1,Interfaces).

get_for_router_name(Interfaces, RName) ->
  lists:keyfind(whereis(RName),4,Interfaces).

get_for_monitor(Interfaces, Monitor) ->
  lists:keyfind(Monitor,5,Interfaces).

get_for_ip(Interfaces, Ip) ->
  lists:keyfind(Ip,2,Interfaces).

get_for_generalized_ip([], _) -> false;
get_for_generalized_ip([{IntName, Ip, Mask, ToDevicePid, Monitor} | T], GenIp) ->
  case ip_utils:generalize_network(Ip, Mask) == GenIp of
    true -> {IntName, Ip, Mask, ToDevicePid, Monitor};
    false -> get_for_generalized_ip(T, GenIp)
  end.

get_for_pid(Interfaces, Pid) ->
  lists:keyfind(Pid,4,Interfaces).

broadcast([], _) -> ok;
broadcast([{_, _, _, ToRouterPid, _} | T], Message) ->
  ToRouterPid ! Message,
  broadcast(T, Message).

print_pretty(Interfaces, RName) ->
  io:format("[~w]: Porty ~n", [RName]),
  io:fwrite("|~-10s|~-16s|~-16s|~-15s|~-15s|~n", ["Name", "Ip", "Mask", "Device Pid", "MonitorRef"]),
  print_entries(Interfaces).

print_entries([]) -> ok;
print_entries([{IntName, Ip, Mask, ToDevicePid, Monitor} | T]) ->
  io:fwrite("|~-10w|~-16s|~-16s|~-15w|~-15w|~n", [IntName, Ip, Mask, ToDevicePid, Monitor]),
  print_entries(T).
