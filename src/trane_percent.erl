%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created : 19 May 2015 by masse <masse@cronqvi.st>

%% @doc
%% encode/decode percent encoded strings according to x-www-form-urlencoded
%% input/output is a string, i.e. [integer(I)] where 0 =< I =< 255.
%%   encode/1
%%      turns spaces into "+"
%%      turns all other reserved characters into "%XX" where "XX" is a hex
%%      Line breaks are represented as "\r\n" pairs (i.e., `%0D%0A')
%%   decode/1
%%      turns "+" into " "
%%      turns "%0D%0A" into "\n"
%%      turns all "%XX" into a Char, where "XX" is the hex value of Char.
%%      if "XX" is not hex, that instance of "%XX" is left untouched.
%% @end

-module('trane_percent').
-author('masse').
-export([encode/1,decode/1]).

-ignore_xref(
   [encode/1,decode/1]).

%% multipart/form-data uses MIME encoding, dafult is text/plain
%%     (http://www.w3.org/TR/html4/interact/forms.html#h-17.13.4.2)
%% application/x-www-form-urlencoded uses +
%%     (http://www.w3.org/TR/html4/interact/forms.html#h-17.13.4.1)
%% properly encoded URIs use %20
%%     (https://www.ietf.org/rfc/rfc3986.txt)

%% unreserved = ALPHA / DIGIT / "-" / "." / "_"
-define(is_unreserved(C),
        $a=<C, C=<$z;
           $A=<C, C=<$Z;
           $0=<C, C=<$9;
           C=:=$-;
           C=:=$.;
           C=:=$_).

-define(is_linebreak(C),
        C=:=$\n; C=:=$\v; C=:=$\f; C=:=$\r).

encode(Str) when is_list(Str) ->
  lists:foldr(fun enc/2,"",Str).

enc($ ,A) ->
  [$+|A];
enc(C,A) when ?is_unreserved(C) ->
  [C|A];
enc(C,A) when ?is_linebreak(C) ->
  [$%,$0,$D,$%,$0,$A|A];
enc(C,A) when 0=<C, C=<255 ->
  <<H1:4/integer,H2:4/integer>> = <<C>>,
  [$%,enchx(H1),enchx(H2)|A];
enc(C,_) ->
  error({bad_char,C}).

enchx(C) when C<10 -> C+$0;
enchx(C)           -> C-10+$A.


decode(Str) when is_list(Str) ->
  dec(Str,[]).

-define(is_cr(C), (C=:=$d orelse C=:=$D)).
-define(is_nl(C), (C=:=$a orelse C=:=$A)).
dec([],A) ->
  lists:reverse(A);
dec([$+|R],A) ->
  dec(R,[$ |A]);
dec([$%,$0,CR,$%,$0,NL|R],A) when ?is_cr(CR),?is_nl(NL) ->
  dec(R,[$\n|A]);
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
   ?_assertEqual("%0D%0A%0D%0A"      ,encode("\r\n")),
   ?_assertEqual("25%2525"           ,encode("25%25")),
   ?_assertEqual("Hatti+fnatti%21"   ,encode("Hatti fnatti!")),
   ?_assertError({bad_char,_}        ,encode([256])),
   ?_assertEqual("%2B"               ,encode("+")),
   ?_assertEqual("%26%3D%2A"         ,encode("&=*"))].

d_test_() ->
  [?_assertEqual("abcABC123"     ,decode("abcABC123")),
   ?_assertEqual("\n"            ,decode("%0D%0A")),
   ?_assertEqual("25%25"         ,decode("25%2525")),
   ?_assertEqual("Hatti fnatti!" ,decode("Hatti%20fnatti%21")),
   ?_assertEqual("Hatti fnatti!" ,decode("Hatti+fnatti%21")),
   ?_assertEqual("+"             ,decode("%2B")),
   ?_assertEqual("27% of 2B"     ,decode("27% of 2B")),
   ?_assertError({bad_char,_}    ,decode([256|"abc"])),
   ?_assertEqual("&=*"           ,decode("%26%3D%2A"))].

-endif.
