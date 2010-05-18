%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created : 10 May 2010 by mats cronqvist <masse@kreditor.se>
%%% Copyright (c) 2010 Mats Cronqvist
%%
%% @doc
%% SAX parser for broken HTML
%% missing:
%% * handle '<' in text
%% * handle attributes in end-tag
%% @end

-module('trane').
-author('mats cronqvist').
-export([sax/3
         , unit/0
         , wget_parse/1,wget_print/1]).

-define(ok(X),$a=<X,X=<$z;$A=<X,X=<$Z;$0=<X,X=<$9;X==$_;X==$-;X==$:).
-define(ws(X),X==$\s ;X==$\r;X==$\n;X==$\t).
-define(endv(S),?ws(hd(S));hd(S)==$>;hd(S)==$/,hd(tl(S))==$>;hd(S)==$=).
-define(dq(Z),Z==$").
-define(sq(Z),Z==$').

sax(Str,Fun,Acc) ->
  parse(Str,Fun,Acc).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% parser
parse(Str,Fun,Acc) ->
  parse({Fun,Acc,[]},tokenize(Str)).

parse(State,[Token1,Token2,T]) ->
  parse(maybe_emit(Token2,maybe_emit(Token1,State)),T);
parse(State,[Token,T]) ->
  parse(maybe_emit(Token,State),T);
parse({Fun,Acc,Stack}=State,{Token,T}) -> 
  case Token of
    eof -> eof(unroll(Stack,{Fun,Acc,[]}));
    _   -> parse(maybe_emit(Token,State),tokenize(T))
  end.

eof({_Fun,Acc,_Stack}) -> Acc.

maybe_emit(Token,State) ->
  case Token of
    {sc,Tag,Attrs}   -> emit({end_tag,Tag},emit({tag,Tag,Attrs},State));
    {open,Tag,Attrs} -> emit({tag,Tag,Attrs},push(Tag,State));
    {close,Tag}      -> try emit({end_tag,Tag},pop(Tag,maybe_unroll(Tag,State)))
                        catch bogus -> State
                        end;
    {comment,Cm}     -> emit({comment,Cm},State);
    {question,DT}    -> emit({question,DT},State);
    {exclamation,DT} -> emit({exclamation,DT},State);
    {text,""}        -> State;
    {text,Text}      -> emit({text,Text},State)
  end.

emit(Token,{Fun,Acc,Stack}) -> {Fun,Fun(Token,Acc),Stack}.

push(Tag,{Fun,Acc,Stack}) -> {Fun,Acc,[Tag|Stack]}.

pop(Tag,{Fun,Acc,[Tag|Stack]}) -> {Fun,Acc,Stack}.

unroll(X,State) ->
  lists:foldl(fun(T,S)->emit({end_tag,T},S)end,State,X).

maybe_unroll(Tag,{Fun,Acc,Stack}) ->
  case lists:member(Tag,Stack) of
    true -> 
      %% close all open tags in this tags scope
      {Hd,Tl} = lists:splitwith(fun(T)-> T=/=Tag end,Stack),
      unroll(Hd,{Fun,Acc,Tl});
    false->
      %% close tag has no open tag; drop it
      throw(bogus)
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% tokenizer
tokenize(Str) when is_integer(hd(Str)) ->
  tokenize(tz(nil,Str));

tokenize({TZ,Str}) ->
  tokenize(tz(TZ,Str));
tokenize({text,Str,Token}) ->
  case ff(text,Str) of
    {{tag,""},EStr,{text,Txt}} -> 
      try [Token,{text,Txt},tokenize({{tag,""},EStr})]
      catch _:_ -> tokenize({text,Txt++"&lt;"++EStr,Token})
      end;
    {{text,Str},"",eof} -> [Token,{eof,{text,Str}}]
  end;
tokenize({TZ,Str,Token}) ->
  {Token,{TZ,Str}}.

tz(nil,"<"++Str)                        -> {{tag,""},ws(Str)};
tz(nil,[_|Str])                         -> {nil,Str};

tz({tag,""},"!--"++Str)                 -> {{comm,""},Str};
tz({tag,""},"!"++Str)                   -> {{excl,""},ws(Str)};
tz({tag,""},"?"++Str)                   -> {{que,""},ws(Str)};
tz({tag,""},"/"++Str)                   -> {{end_tag,""},ws(Str)};
tz({tag,Tag},Str) when ?endv(Str)       -> {{attr,"",{Tag,[]}},ws(Str)};
tz({tag,Tag},[X|Str])when ?ok(X)        -> {{tag,Tag++[X]},Str};

tz({excl,DT},">"++Str)                  -> {text,Str,{exclamation,dc(DT)}};
tz({excl,DT},[X|Str])                   -> {{excl,DT++[X]},Str};

tz({que,DT},"?>"++Str)                  -> {text,Str,{question,dc(DT)}};
tz({que,DT},[X|Str])                    -> {{que,DT++[X]},Str};

tz({comm,Comm},"-->"++Str)              -> {text,Str,{comment,Comm}};
tz({comm,Comm},[X|Str])                 -> {{comm,Comm++[X]},Str};

tz({etag,Tag,Attrs},"/>"++Str)          -> {text,Str,{sc,Tag,Attrs}};
tz({etag,"script",Attrs},">"++Str)      -> {script,Str,{open,"script",Attrs}};
tz({etag,"style",Attrs},">"++Str)       -> {style,Str,{open,"style",Attrs}};
tz({etag,Tag,Attrs},">"++Str)           -> {text,Str,{open,Tag,Attrs}};

tz({end_tag,Tag},Str) when ?endv(Str)   -> {{end_tag,Tag,'>'},ws(Str)};
tz({end_tag,Tag,'>'},">"++Str)          -> {text,Str,{close,dc(Tag)}};
tz({end_tag,Tag},[X|Str])               -> {{end_tag,Tag++[X]},Str};

tz({attr,"",{Tag,As}},S)when ?endv(S)   -> {{etag,dc(Tag),As},S};
tz({attr,A,{T,As}},S)when ?endv(S)      -> {{eatt,{dc(A),T,As}},ws(S)};
tz({attr,A,TAs},[X|Str])when ?ok(X)     -> {{attr,A++[X],TAs},Str};

tz({eatt,ATAs},"="++Str)                -> {{val,ATAs},ws(Str)};
tz({eatt,{A,T,As}},S)                   -> {{attr,"",{T,As++[{A,""}]}},ws(S)};

tz({val,ATAs},[X|Str])when ?sq(X)       -> {{sqval,"",ATAs},Str};%singlequoted
tz({val,ATAs},[X|Str])when ?dq(X)       -> {{dqval,"",ATAs},Str};%doublequoted
tz({val,ATAs},Str)                      -> {{uqval,"",ATAs},Str};%unquoted

tz({sqval,V,{A,T,As}},[X|S])when ?sq(X) -> {{attr,"",{T,As++[{A,V}]}},ws(S)};
tz({sqval,V,ATAs},[X|Str])              -> {{sqval,V++[X],ATAs},Str};

tz({dqval,V,{A,T,As}},[X|S])when ?dq(X) -> {{attr,"",{T,As++[{A,V}]}},ws(S)};
tz({dqval,V,ATAs},[X|Str])              -> {{dqval,V++[X],ATAs},Str};

tz({uqval,V,{A,T,As}},S)when ?endv(S)   -> {{attr,"",{T,As++[{A,V}]}},ws(S)};
tz({uqval,V,ATAs},[X|Str])              -> {{uqval,V++[X],ATAs},Str};

tz(script,Str)                          -> ff(script,Str);
tz(style,Str)                           -> ff(style,Str);
tz(text,Str)                            -> ff(text,Str);

tz(X,"")                                -> {X,"",eof}.

ff(What,Str) ->
  case ff(Str,ff(What),[1,2]) of
    [Scr,Rest] -> {{tag,""},ws(Rest),{text,Scr}};
    [] -> {{text,Str},"",eof}
  end.

ff(script)-> "(.*)<(\\s*/\\s*script\\s*>.*)\$";
ff(style) -> "(.*)<(\\s*/\\s*style\\s*>.*)\$";
ff(text)  -> "(.*)<(.*)\$".

ff(Str,P,Groups) ->
  case re:run(Str,P,[{capture,Groups,list},dotall,caseless,ungreedy]) of
    {match,Ms} -> Ms;
    nomatch -> []
  end.

ws([X|Str]) when ?ws(X) -> ws(Str);
ws(Str) -> Str.

dc(Str) -> string:to_lower(Str).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ad-hoc unit test
unit() ->
  validate(
    [{"<!DOCTYPE bla><P a=b c=d>",
      [{exclamation,"doctype bla"},
       {tag,"p",[{"a","b"},{"c","d"}]},
       {end_tag,"p"}]},
     {"<head><B>< p><p ></z></ b></x>",
      [{tag,"head",[]},
       {tag,"b",[]},
       {tag,"p",[]},
       {tag,"p",[]}, 
       {end_tag,"p"},
       {end_tag,"p"},
       {end_tag,"b"},
       {end_tag,"head"}]},
     {"<tag catt xatt=\"\" batt>",
      [{tag,"tag",[{"catt",""},{"xatt",""},{"batt",""}]},
       {end_tag,"tag"}]},
     {"<a>...<...</a>",
      [{tag,"a",[]},
       {text,"...&lt;..."},
       {end_tag,"a"}]},
     {"<P a=b c=d>hej<!-- tobbe --><b>svejsan</b>foo</p>grump<x x=y />",
      [{tag,"p",[{"a","b"},{"c","d"}]},
       {text,"hej"},
       {comment," tobbe "},
       {tag,"b",[]},
       {text,"svejsan"},
       {end_tag,"b"},
       {text,"foo"},
       {end_tag,"p"},
       {text,"grump"},
       {tag,"x",[{"x","y"}]},
       {end_tag,"x"}]}
    ]).

validate([]) -> [];
validate([{Str,Toks}|Vs]) -> 
  [try Toks = sax(Str,fun(T,A)-> A++[T] end, []),Str
   catch C:R -> {C,R,erlang:get_stacktrace(),Str}
   end|validate(Vs)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Parsing real pages
wget(Url) ->
  inets:start(),
  {ok, {_Rc, _Hdrs, Body}} = http:request(get, {Url, []}, [], []),
  Body.

wget_parse(Url) ->
  sax(wget(Url),fun(T,A)-> A++[T] end, []).

wget_print(Url) ->
  io:fwrite("~s~n",[wget(Url)]).
