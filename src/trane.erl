%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created : 10 May 2010 by mats cronqvist <masse@kreditor.se>
%%% Copyright (c) 2010 Mats Cronqvist
%%
%% @doc
%% SAX parser for broken HTML
%% missing:
%% * handle attributes in end-tag (?)
%% @end

-module('trane').
-author('mats cronqvist').
-export([sax/3
         , unit/0,unit/1
         , file_parse/1
         , wget_parse/1,wget_print/1]).

sax(Str,Fun,Acc) ->
  parse(Str,Fun,Acc).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% parser
parse(Str,Fun,Acc) ->
  parse({Fun,Acc,[]},tokenize(Str)).

parse({Fun,Acc,Stack}=State,[{Token,T}]) -> 
  case Token of
    eof -> eof(unroll(Stack,{Fun,Acc,[]}));
    _   -> parse(maybe_emit(Token,State),tokenize(T))
  end;
parse(State,[Token|Ts]) ->
  parse(maybe_emit(Token,State),Ts).

eof({_Fun,Acc,_Stack}) -> Acc.

maybe_emit(Token,State) ->
  case Token of
    {sc,Tag,Attrs}   -> emit({end_tag,Tag},emit({tag,Tag,Attrs},State));
    {open,Tag,Attrs} -> emit({tag,Tag,Attrs},push(Tag,State));
    {close,Tag}      -> try emit({end_tag,Tag},pop(Tag,maybe_unroll(Tag,State)))
                        catch bogus -> State
                        end;
    {comment,Cm}     -> emit({comment,Cm},State);
    {'?',DT}    -> emit({'?',DT},State);
    {'!',DT} -> emit({'!',DT},State);
    {text,<<>>}        -> State;
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

%tz(<<>>,O)->lists:reverse(O);
%   (<<X:8/integer,R/binary>>,O)->tz(R,[X|O])
%end.


-define(m(X,B),<<X,B/binary>>). % match
-define(d(X,B),<<X:8/integer,B/binary>>). % decompose
-define(M(X,B),<<X,_/binary>> = B). % match
-define(D(X,B),<<X:8/integer,_/binary>> = B). % decompose

-define(ok(X),$a=<X,X=<$z;$A=<X,X=<$Z;$0=<X,X=<$9;X==$_;X==$-;X==$:).
-define(ws(X),X==$\s;X==$\r;X==$\n;X==$\t).
-define(dq(X),X==$").
-define(sq(X),X==$').

-define(ev(X), ?ws(X); X==$>; X==$=).

tokenize(Str) when is_integer(hd(Str))-> tokenize(list_to_binary(Str));
tokenize(Str) when is_binary(Str) -> 
  case ff(text,Str) of
    {{tag,""},EStr,{text,Txt}} -> tokenize({text,EStr,{text,Txt}});
    {{text,Str},"",eof} -> [{eof,{text,Str}}]
  end;

tokenize({text,Str,Token}) ->
  case ff(text,Str) of
    {{tag,""},EStr,{text,Txt}} -> 
      try [Token,{text,Txt}]++tokenize(t({tag,""},EStr))
      catch _:_ -> TB = list_to_binary(Txt),
                   tokenize({text,<<TB/binary,"&lt;",EStr/binary>>,Token})
      end;
    {{text,Txt},"",eof} ->
      [Token,{eof,{text,Txt}}]
  end;
tokenize({TZ,Str,Token}) ->
  [{Token,{TZ,Str}}].

t({tag,""},?m("!--",Str))                -> t({comm,""},Str);
t({tag,""},?m("!",Str))                  -> t({'!',""},ws(Str));
t({tag,""},?m("?",Str))                  -> t({que,""},ws(Str));
t({tag,""},?m("/",Str))                  -> t({end_tag,""},ws(Str));
t({tag,Tag},?D(X,S))when ?ev(X)          -> t({attr,"",{Tag,[]}},ws(S));
t({tag,Tag},?d(X,Str))when ?ok(X)        -> t({tag,Tag++[X]},Str);

t({'!',DT},?m(">",Str))                  -> {text,Str,{'!',dc(DT)}};
t({'!',DT},?d(X,Str))                    -> t({'!',DT++[X]},Str);

t({que,DT},?m("?>",Str))                 -> {text,Str,{'?',dc(DT)}};
t({que,DT},?d(X,Str))                    -> t({que,DT++[X]},Str);

t({comm,Comm},?m("-->",Str))             -> {text,Str,{comment,Comm}};
t({comm,Comm},?d(X,Str))                 -> t({comm,Comm++[X]},Str);

t({etag,Tag,Attrs},?m("/>",Str))         -> {text,Str,{sc,Tag,Attrs}};
t({etag,"script",Attrs},?m(">",Str))     -> {script,Str,{open,"script",Attrs}};
t({etag,"style",Attrs},?m(">",Str))      -> {style,Str,{open,"style",Attrs}};
t({etag,Tag,Attrs},?m(">",Str))          -> {text,Str,{open,Tag,Attrs}};

t({end_tag,Tag},?D(X,S))when ?ev(X)      -> t({end_tag,Tag,'>'},ws(S));
t({end_tag,Tag,'>'},?m(">",Str))         -> {text,Str,{close,dc(Tag)}};
t({end_tag,Tag},?d(X,Str))               -> t({end_tag,Tag++[X]},Str);

t({attr,"",{Tag,As}},?D(X,S))when ?ev(X) -> t({etag,dc(Tag),As},S);
t({attr,"",{Tag,As}},?M("/>",S))         -> t({etag,dc(Tag),As},S);
t({attr,A,{T,As}},?D(X,S))when ?ev(X)    -> t({eatt,{dc(A),T,As}},ws(S));
t({attr,A,{T,As}},?M("/>",S))            -> t({eatt,{dc(A),T,As}},ws(S));
t({attr,A,TAs},?d(X,Str))when ?ok(X)     -> t({attr,A++[X],TAs},Str);

t({eatt,ATAs},?m("=",Str))               -> t({val,ATAs},ws(Str));
t({eatt,{A,T,As}},S)                     -> t({attr,"",{T,As++[{A,""}]}},ws(S));

t({val,ATAs},?m("'",Str))                -> t({sqval,"",ATAs},Str);%singlequ
t({val,ATAs},?m("\"",Str))               -> t({dqval,"",ATAs},Str);%doublequ
t({val,ATAs},Str)                        -> t({uqval,"",ATAs},Str);%unquoted

t({sqval,V,{A,T,As}},?m("'",S))          -> t({attr,"",{T,As++[{A,V}]}},ws(S));
t({sqval,V,ATAs},?d(X,Str))              -> t({sqval,V++[X],ATAs},Str);

t({dqval,V,{A,T,As}},?m("\"",S))         -> t({attr,"",{T,As++[{A,V}]}},ws(S));
t({dqval,V,ATAs},?d(X,Str))              -> t({dqval,V++[X],ATAs},Str);

t({uqval,V,{A,T,As}},?D(X,S))when ?ev(X) -> t({attr,"",{T,As++[{A,V}]}},ws(S));
t({uqval,V,{A,T,As}},?M("/>",S))         -> t({attr,"",{T,As++[{A,V}]}},ws(S));
t({uqval,V,ATAs},?d(X,Str))              -> t({uqval,V++[X],ATAs},Str);

t(script,Str)                            -> ff(script,Str);
t(style,Str)                             -> ff(style,Str);
t(text,Str)                              -> ff(text,Str);

t(X,"")                                  -> {X,"",eof}.

ff(What,Str) -> ff(What,Str,[]).

ff(What,<<>>,A) -> {{text,lists:reverse(A)},"",eof};
ff(What,?m("<",Str),A) -> {{tag,""},Str,{text,lists:reverse(A)}};
ff(What,?d(H,Str),A) -> ff(What,Str,[H|A]).

% ff(What,Str) ->
%   case ff(Str,ff(What),[1,2]) of
%     [Scr,Rest] -> {{tag,""},ws(Rest),{text,Scr}};
%     [] -> {{text,Str},"",eof}
%   end.

% ff(script)-> "(.*)<(\\s*/\\s*script\\s*>.*)\$";
% ff(style) -> "(.*)<(\\s*/\\s*style\\s*>.*)\$";
% ff(text)  -> "(.*)<(.*)\$".

% ff(Str,P,Groups) ->
%   case re:run(Str,P,[{capture,Groups,binary},dotall,caseless,ungreedy]) of
%     {match,Ms} -> Ms;
%     nomatch -> []
%  end.

ws(?d(X,Str)) when ?ws(X) -> ws(Str);
ws(Str) -> Str.

dc(Str) -> string:to_lower(Str).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Parsing real pages
wget(Url) ->
  inets:start(),
  {ok, {_Rc, _Hdrs, Body}} = http:request(get, {Url, []}, [], []),
  Body.

file_parse(File) ->
  {ok,B} = file:read_file(File),
  sax(B,fun(T,A)-> [T|A] end, []).

wget_parse(Url) ->
  sax(wget(Url),fun(T,A)-> A++[T] end, []).

wget_print(Url) ->
  io:fwrite("~s~n",[wget(Url)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ad-hoc unit test
unit(N) when is_integer(N) ->
  validate([lists:nth(N,tests())]).

unit() ->
  validate(tests()).

validate([]) -> [];
validate([{Str,Toks}|Vs]) -> 
  [try Toks = sax(Str,fun(T,A)-> A++[T] end, []),Str
   catch C:R -> {C,R,erlang:get_stacktrace(),Str}
   end|validate(Vs)].


tests() ->
  [{"<!DOCTYPE bla><P a=b c=d>",
    [{'!',"doctype bla"},
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
   {"<a href=/a/bc/d.e>x</a>",
    [{tag,"a",[{"href","/a/bc/d.e"}]},
     {text,<<"x">>},
     {end_tag,"a"}]},
   {"<a>...<...</a>",
    [{tag,"a",[]},
     {text,<<"...&lt;...">>},
     {end_tag,"a"}]},
   {"<P a=b c=d>hej<!-- tobbe --><b>svejsan</b>foo</p>grump<x x=y />",
    [{tag,"p",[{"a","b"},{"c","d"}]},
     {text,<<"hej">>},
     {comment," tobbe "},
     {tag,"b",[]},
     {text,<<"svejsan">>},
     {end_tag,"b"},
     {text,<<"foo">>},
     {end_tag,"p"},
     {text,<<"grump">>},
     {tag,"x",[{"x","y"}]},
     {end_tag,"x"}]}
  ].
