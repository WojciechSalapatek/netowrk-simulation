%%%-------------------------------------------------------------------
%%% @author wojciech
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Jan 2020 12:31
%%%-------------------------------------------------------------------
-module(ui).
-author("wojciech").

%% API
-export([start_router/1, stop/1, get_info/1, connect_router_to_router/8, disconnect_router_from_router/2, kill/1, testit/0,
  send_msg_from_router/4, send_msg_from_router/5, comp_topology/0, start_switch/1,
  connect_switch_to_router/6, initialize/0, table_endline/0, start_host/4, switching/0, send_msg_from_host/3]).

initialize() -> ok.

table_endline() ->
  io:fwrite("-------------------------------------------------------------------------------------~n", []).

start_router(Name) ->
  register(Name, spawn(fun() -> router:create_router(Name) end)).

start_switch(Name) ->
  register(Name, spawn(fun() -> switch:create_switch(Name) end)).

start_host(Name, SName, SPort, Ip) ->
  SPid = whereis(SName),
  register(Name, spawn(fun() -> host:create_host(Name, SPid, Ip) end)),
  SPid ! {connect, SPort, Name}.


stop(Name) ->
  whereis(Name) ! stop,
  unregister(Name).

get_info(Name) ->
  whereis(Name) ! info.

kill(Name) ->
  exit(whereis(Name)).

connect_router_to_router(R1Name,R1Interface, R1Ip, Mask, R2Name, R2Interface, R2Ip, Mask) when R1Ip /= R2Ip ->
  R1Pid = whereis(R1Name),
  R2Pid = whereis(R2Name),
  R1Pid ! {connect, R1Interface, R1Ip, Mask, R2Name},
  R2Pid ! {connect, R2Interface, R2Ip, Mask, R1Name}.

disconnect_router_from_router(R1Name, R2Name) ->
  R1Pid = whereis(R1Name),
  R2Pid = whereis(R2Name),
  R1Pid ! {disconnect_router, R2Name},
  R2Pid ! {disconnect_router, R1Name}.


connect_switch_to_router(RName, RInterface, RIp, Mask, SName, SPort) ->
  RPid = whereis(RName),
  SPid = whereis(SName),
  SPid ! {connect, SPort, RName},
  SPid ! {set_gateway, RIp},
  RPid ! {connect, RInterface, RIp, Mask, SName}.

send_msg_from_router(From, Port, ToIp, Msg) -> send_msg_from_router(From, Port, ToIp, Msg, 15).
send_msg_from_router(From, Port, ToIp, Msg, TTL) ->
  R1Pid = whereis(From),
  R1Pid ! {create_msg, Port, ToIp, Msg, TTL}.


send_msg_from_host(HName, ToIp, Msg) ->
  whereis(HName) ! {create_msg, ToIp, Msg, 23}.

testit() ->
  start_router(r1),
  start_router(r2),
  start_router(r3),
  connect_router_to_router(r1, p1, "200.0.0.1", "255.255.255.0", r2, p1, "200.0.0.2", "255.255.255.0"),
  connect_router_to_router(r2, p2, "201.0.0.1", "255.255.255.0", r3, p1, "201.0.0.2", "255.255.255.0").


comp_topology() ->
  start_router(r1),
  start_router(r2),
  start_router(r3),
  start_router(r4),
  start_router(r5),
  start_router(r6),
  start_router(r7),
  start_router(r8),
  start_router(r9),
  start_switch(s1),
  start_switch(s2),
  start_host(h1, s1, p10, "212.0.0.10"),
  start_host(h2, s2, p10, "211.0.0.10"),
  start_host(h3, s2, p11, "211.0.0.11"),
  connect_router_to_router(r1, p1, "200.0.0.1", "255.255.255.0", r2, p1, "200.0.0.2", "255.255.255.0"),
  connect_router_to_router(r2, p2, "201.0.0.1", "255.255.255.0", r3, p1, "201.0.0.2", "255.255.255.0"),
  connect_router_to_router(r3, p2, "202.0.0.1", "255.255.255.0", r4, p1, "202.0.0.2", "255.255.255.0"),
  connect_router_to_router(r4, p2, "203.0.0.1", "255.255.255.0", r6, p1, "203.0.0.2", "255.255.255.0"),
  connect_router_to_router(r3, p3, "204.0.0.1", "255.255.255.0", r5, p1, "204.0.0.2", "255.255.255.0"),
  connect_router_to_router(r2, p3, "205.0.0.1", "255.255.255.0", r6, p2, "205.0.0.2", "255.255.255.0"),
  connect_router_to_router(r1, p2, "206.0.0.1", "255.255.255.0", r9, p1, "206.0.0.2", "255.255.255.0"),
  connect_router_to_router(r1, p3, "207.0.0.1", "255.255.255.0", r7, p1, "207.0.0.2", "255.255.255.0"),
  connect_router_to_router(r7, p2, "208.0.0.1", "255.255.255.0", r8, p1, "208.0.0.2", "255.255.255.0"),
  connect_router_to_router(r8, p2, "210.0.0.1", "255.255.255.0", r9, p2, "210.0.0.2", "255.255.255.0"),
  connect_router_to_router(r4, p3, "209.0.0.1", "255.255.255.0", r9, p3, "209.0.0.2", "255.255.255.0"),
  connect_switch_to_router(r4, p4, "211.0.0.1", "255.255.255.0", s2, p1),
  connect_switch_to_router(r1, p4, "212.0.0.1", "255.255.255.0", s1, p1).

switching()->
  start_switch(s1),
  whereis(s1) ! {set_gateway, "200.0.0.1"},
  start_host(h1, s1, p1, "200.0.0.5"),
  start_host(h2, s1, p2, "200.0.0.9").
