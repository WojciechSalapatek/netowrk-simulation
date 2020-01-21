%%%-------------------------------------------------------------------
%%% @author wojciech
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Jan 2020 19:10
%%%-------------------------------------------------------------------
-module(mac_table).
-author("wojciech").

%% API
-export([create/0, print_pretty/2, add/3, get_for_mac/2, get_for_port/2]).


create() -> [].

add(MacTable, Port, Mac) ->
  case lists:keyfind(Port, 1, MacTable) of
    {_, Mac} ->
      Macs = lists:keydelete(Mac, 2, MacTable),
      [{Port, Mac} | Macs];
    false ->
      [{Port, Mac} | MacTable]
  end.

get_for_mac(MacTable, Mac) ->
  lists:keyfind(Mac, 2, MacTable).

get_for_port(MacTable, Port) ->
  lists:keyfind(Port, 1, MacTable).

print_pretty(MacTable, Name) ->
  io:format("[~w]: Mac Table ~n", [Name]),
  io:fwrite("~-10s|~-26s|~n", ["Port", "Mac address"]),
  print_entries(MacTable).

print_entries([]) -> ok;
print_entries([{Port, Mac} | T]) ->
  io:fwrite("~-10w|~-26s|~n", [Port, Mac]),
  print_entries(T).
