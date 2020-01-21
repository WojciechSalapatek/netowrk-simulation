%%%-------------------------------------------------------------------
%%% @author wojciech
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Jan 2020 20:33
%%%-------------------------------------------------------------------
-module(host).
-author("wojciech").

%% API
-export([create_host/3]).


create_host(Name, SPid, Ip) ->
  host(Name, SPid, Ip).

host(Name, SPid, Ip) ->
  receive
    info -> io:format("[~w]: Host ~w with ip ~s under ~w~n",[Name, Name, Ip, SPid]);
    {mac_req, Name, SPid} ->
      SPid ! {mac_answ, Name, self()},
      io:format("[~w]: Recieved matching mac request, sending response<~w>~n",[Name, Name]),
      host(Name, SPid, Ip);
    {arp_req, Ip, SPid} ->
      io:format("[~w]: Recieved matching arp request, sending response<~s,~w>~n",[Name, Ip, Name]),
      SPid ! {arp_answ, Ip, Name, self()},
      host(Name, SPid, Ip);
    {msg, Ip, Name, Msg, FromIp, FromMac, _} ->
      io:format("[~w]: Recieved message ~s at host <~s, ~w> from <~s, ~w>~n", [Name, Msg, Ip, Name, FromIp, FromMac]),
      host(Name, SPid, Ip);
    {create_msg, ToIp, Msg, TTL} ->
      io:format("[~w]: Sending message ~s from host <~s, ~w> to <~s, ~w>~n", [Name, Msg, Ip, Name, ToIp, undefined]),
      SPid ! {msg, ToIp, undefined, Msg, Ip, Name, TTL},
      host(Name, SPid, Ip);
    stop -> ok
  end.
