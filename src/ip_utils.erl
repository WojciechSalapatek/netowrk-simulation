%%%-------------------------------------------------------------------
%%% @author wojciech
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Jan 2020 17:39
%%%-------------------------------------------------------------------
-module(ip_utils).
-author("wojciech").

%% API
-export([correctv4/1, cut_octets/2, get_mask_octets/1, generalize_network/2, mask_from_ip/1]).


ipv4_regex() ->
  "^(([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])\.){3}([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])$".

correctv4(Ip) ->
  case re:run(Ip, ipv4_regex()) of
    {match, _} -> match;
    nomatch -> nomatch
  end.

cut_octets(Ip, 0) -> Ip;
cut_octets(Ip, 1) ->
  re:replace(Ip, "[0-9]{1,3}$", "0", [{return, list}]);
cut_octets(Ip, 2) ->
  re:replace(Ip, "[0-9]{1,3}\\.[0-9]{1,3}$", "0.0", [{return, list}]);
cut_octets(Ip, 3) ->
  re:replace(Ip, "[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}$", "0.0.0", [{return, list}]).


get_mask_octets("255.255.255.255") -> 0;
get_mask_octets("255.255.255.0") -> 1;
get_mask_octets("255.255.0.0") -> 2;
get_mask_octets("255.0.0.0") -> 3;
get_mask_octets("0.0.0.0") -> 4.

mask_from_ip("0.0.0.0") -> "255.255.255.255";
mask_from_ip(Ip) ->
  case re:run(Ip, "^([1-9]\\.|[1-9][0-9]\\.|1[0-1][0-9]\\.|12[0-6]\\.)([0-9]{1,3})\\.([0-9]{1,3})\\.([0-9]{1,3})$") of
    {match, _} -> "255.0.0.0";
    nomatch ->
      case re:run(Ip, "^(12[8-9]\\.|1[3-8][0-9]\.|19[0-1]\\.)([0-9]{1,3})\\.([0-9]{1,3})\\.([0-9]{1,3})$") of
        {match, _} -> "255.255.0.0";
        nomatch -> "255.255.255.0"
      end

  end.

generalize_network(Ip, Mask) ->
  cut_octets(Ip, ip_utils:get_mask_octets(Mask)).
