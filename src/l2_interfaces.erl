%%%-------------------------------------------------------------------
%%% @author wojciech
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Jan 2020 18:30
%%%-------------------------------------------------------------------
-module(l2_interfaces).
-author("wojciech").

%% API
-export([create/0, connect/4, print_pretty/2, broadcast/2, get_for_name/2, send/3, get_for_pid/2]).

create() -> [].

connect(Interfaces, IntName, ToDevicePid, Monitor) ->
  case lists:keyfind(IntName, 1, Interfaces) of
    {IntName, _, OldMonitor} ->
      erlang:demonitor(OldMonitor),
      Ints = lists:keydelete(IntName, 1, Interfaces),
      [{IntName, ToDevicePid, Monitor} | Ints];
    false ->
      [{IntName, ToDevicePid, Monitor} | Interfaces]
  end.

get_for_name(Interfaces, Name) ->
  lists:keyfind(Name, 1, Interfaces).

get_for_pid(Interfaces, Pid) ->
  lists:keyfind(Pid, 2, Interfaces).

send(Interfaces, Port, Msg) ->
  {_, Pid, _} = lists:keyfind(Port, 1, Interfaces),
  Pid ! Msg.

broadcast([], _) -> ok;
broadcast([{_, Pid, _} | T], Msg) ->
  Pid ! Msg,
  broadcast(T, Msg).

print_pretty(Interfaces, SName) ->
  io:format("[~w]: Porty ~n", [SName]),
  io:fwrite("|~-10s|~-16s|~-15s|~n", ["Name", "Device Pid", "MonitorRef"]),
  print_entries(Interfaces).

print_entries([]) -> ok;
print_entries([{IntName, ToDevicePid, Monitor} | T]) ->
  io:fwrite("|~-10w|~-16w|~-15w|~n", [IntName, ToDevicePid, Monitor]),
  print_entries(T).
