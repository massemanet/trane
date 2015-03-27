%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created : 27 Mar 2015 by  <masse@klarna.com>

%% @doc
%% @end

-module('trane_poster').
-author('').
-export([go/0,go/1]).

go() ->
  go("http://google.com").
go(URL) ->
  inets:start(),
  {ok,{{_,200,_},_Headers,Body}} = httpc:request(get,{URL,[]},[],[]),
  trane:sax(Body,fun formf/2,ok).

formf({tag,"form",C},_)        -> {go,[C]};
formf({end_tag,"form"},{go,A}) -> lists:reverse(A);
formf({tag,"input",T},{go,A})  -> {go,[T|A]};
formf(_,A)                     -> A.
