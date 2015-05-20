%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created : 19 May 2015 by masse <masse@cronqvi.st>

%% @doc
%% encode/decode percent encoded strings according to RFC3986
%% input/output is a string, i.e. [integer(I)] where 0 =< I =< 255.
%%   encode/1 turns all unreserved characters into "%XX" where "XX" is a hex
%%   decode/1 turns all "%XX" into a Char, where "XX" is the hex value of Char.
%% if "XX" is not hex, that instance of "%XX" is left untouched.
%% @end

-module('trane_percent').
-author('masse').
-export([encode/1,decode/1]).

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

enc(C,A) when ?is_unreserved(C) ->
  [C|A];
enc(C,A) when 0=<C, C=<255 ->
  <<H1:4/integer,H2:4/integer>> = <<C>>,
  [$%,enchx(H1),enchx(H2)|A];
enc(C,_) ->
  error({bad_char,C}).

enchx(C) when C<10 -> C+$0;
enchx(C)           -> C-10+$A.


decode(Str) when is_list(Str) ->
  dec(Str,[]).

dec([],A) ->
  lists:reverse(A);
dec([$%,C1,C2|R],A) ->
  case dechx(C1,C2) of
    fail -> dec([C1,C2|R],[$%|A]);
    Char -> dec(R,[Char|A])
  end;
dec([C|R],A) when 0=<C, C=<255 ->
  dec(R,[C|A]);
dec(C,_) ->
  error({bad_char,C}).

dechx(C1,C2) ->
  case {dechx(C1),dechx(C2)} of
    {fail,_} -> fail;
    {_,fail} -> fail;
    {I1,I2}  -> <<I:8/integer>> = <<I1:4/integer,I2:4/integer>>, I
  end.

dechx(C) when $a=<C,C=<$f -> C-$a+10;
dechx(C) when $A=<C,C=<$F -> C-$a+10;
dechx(C) when $0=<C,C=<$9 -> C-$0;
dechx(_) -> fail.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TESTING

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

e_test_() ->
  [?_assertEqual("abcABC123"         ,encode("abcABC123")),
   ?_assertEqual("%0D%0A"            ,encode("\r\n")),
   ?_assertEqual("25%2525"           ,encode("25%25")),
   ?_assertEqual("Hatti%20fnatti%21" ,encode("Hatti fnatti!")),
   ?_assertError({bad_char,_}        ,encode([256])),
   ?_assertEqual("%2B"               ,encode("+")),
   ?_assertEqual("%26%3D%2A"         ,encode("&=*"))].

d_test_() ->
  [?_assertEqual("abcABC123"     ,decode("abcABC123")),
   ?_assertEqual("\r\n"          ,decode("%0D%0A")),
   ?_assertEqual("25%25"         ,decode("25%2525")),
   ?_assertEqual("Hatti fnatti!" ,decode("Hatti%20fnatti%21")),
   ?_assertEqual("+"             ,decode("%2B")),
   ?_assertEqual("27% of 2B"     ,decode("27% of 2B")),
   ?_assertError({bad_char,_}    ,decode([256|"abc"])),
   ?_assertEqual("&=*"           ,decode("%26%3D%2A"))].

-endif.
