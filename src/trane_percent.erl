%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created : 19 May 2015 by masse <masse@cronqvi.st>

%% @doc
%% @end

-module('trane_percent').
-author('masse').
-export([encode/1]).

%% rfc 3986
%% unreserved = ALPHA / DIGIT / "-" / "." / "_" / "~"
-define(is_unreserved(C),
        $a=<C, C=<$z;
          $A=<C, C=<$Z;
          $0=<C, C=<$9;
          C=:=$-;
          C=:=$.;
          C=:=$_;
          C=:=$~).

encode(Str) when is_list(Str) ->
  lists:foldr(fun enc/2,"",Str).

enc(C,A) when ?is_unreserved(C) -> [C|A];
enc(C,A) ->
  <<H1:4/integer,H2:4/integer>> = <<C>>,
  [$%,enchx(H1),enchx(H2)|A].

enchx(C) when C<10 -> C+$0;
enchx(C)           -> C-10+$A.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TESTING

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

f_test_() ->
  [?_assertEqual("abcABC123"         , encode("abcABC123")),
   ?_assertEqual("%0D%0A"            , encode("\r\n")),
   ?_assertEqual("25%2525"           , encode("25%25")),
   ?_assertEqual("Hatti%20fnatti%21" , encode("Hatti fnatti!")),
   ?_assertEqual("%2B"               , encode("+")),
   ?_assertEqual("%26%3D%2A"         , encode("&=*"))].

-endif.
