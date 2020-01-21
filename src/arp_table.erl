%%%-------------------------------------------------------------------
%%% @author wojciech
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Jan 2020 16:31
%%%-------------------------------------------------------------------
-module(arp_table).
-author("wojciech").

%% API
-export([create/0, add/3, get_for_ip/2, get_for_mac/2, print_pretty/2, remove_ip/2, remove_mac/2]).

create() -> [].

add(ATable, Ip, Mac) ->
  case lists:keyfind(Ip, 1, ATable) of
    {_, Mac} ->
      Arps = lists:keydelete(Mac, 2, ATable),
      [{Ip, Mac} | Arps];
    false ->
      [{Ip, Mac} | ATable]
  end.

get_for_ip(ATable, Ip) ->
  lists:keyfind(Ip, 1, ATable).

get_for_mac(ATable, Mac) ->
  lists:keyfind(Mac, 2, ATable).

remove_mac(ATable, Mac) ->
  lists:keydelete(Mac, 2, ATable).

remove_ip(ATable, Ip) ->
  lists:keydelete(Ip, 1, ATable).

print_pretty(ATable, Name) ->
  io:format("[~w]: Arp Table ~n", [Name]),
  io:fwrite("~-16s|~-26s|~n", ["Ip", "Mac address"]),
  print_entries(ATable).

print_entries([]) -> ok;
print_entries([{Ip, Mac} | T]) ->
  io:fwrite("~-16s|~-26w|~n", [Ip, Mac]),
  print_entries(T).
