% -*- mode: erlang; erlang-indent-level: 2 -*-
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
-export([sax/3, wget_parse/1, wget_print/1, wget_sax/3]).

-ignore_xref(
   [sax/3,wget_parse/1,wget_print/1]).

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
    {close,Tag}      -> emit_end_tag(Tag,State);
    {comment,Cm}     -> emit({comment,Cm},State);
    {'?',DT}         -> emit({'?',DT},State);
    {'!',DT}         -> emit({'!',DT},State);
    {text,<<>>}      -> State;
    {text,Text}      -> emit({text,Text},State)
  end.

emit_end_tag(Tag,State) ->
  try emit({end_tag,Tag},pop(Tag,maybe_unroll(Tag,State)))
  catch bogus -> State
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
tokenize(Str) when is_binary(Str) -> tokenize(tz(nil,Str));

tokenize({TZ,Str}) ->
  tokenize(tz(TZ,Str));
tokenize({text,Str,Token}) ->
  case ff(text,Str) of
    {{tag,""},EStr,{text,Txt}} ->
      try [Token,{text,Txt}]++tokenize({{tag,""},EStr})
      catch _:_ -> tokenize({text,<<Txt/binary,"&lt;",EStr/binary>>,Token})
      end;
    {{text,Str},"",eof} ->
      [Token,{eof,{text,Str}}]
  end;
tokenize({TZ,Str,Token}) ->
  [{Token,{TZ,Str}}].

tz(nil,?m("<",Str))                       -> {{tag,""},ws(Str)};
tz(nil,?d(_,Str))                         -> {nil,Str};

tz({tag,""},?m("!--",Str))                -> {{comm,""},Str};
tz({tag,""},?m("!",Str))                  -> {{'!',""},ws(Str)};
tz({tag,""},?m("?",Str))                  -> {{que,""},ws(Str)};
tz({tag,""},?m("/",Str))                  -> {{end_tag,""},ws(Str)};
tz({tag,Tag},?m("/>",Str))                -> {text,Str,{sc,Tag,[]}};
tz({tag,Tag},?D(X,S))when ?ev(X)          -> {{attr,"",{Tag,[]}},ws(S)};
tz({tag,Tag},?d(X,Str))when ?ok(X)        -> {{tag,Tag++[X]},Str};

tz({'!',DT},?m(">",Str))                  -> {text,Str,{'!',dc(DT)}};
tz({'!',DT},?d(X,Str))                    -> {{'!',DT++[X]},Str};

tz({que,DT},?m("?>",Str))                 -> {text,Str,{'?',dc(DT)}};
tz({que,DT},?d(X,Str))                    -> {{que,DT++[X]},Str};

tz({comm,Comm},?m("-->",Str))             -> {text,Str,{comment,Comm}};
tz({comm,Comm},?d(X,Str))                 -> {{comm,Comm++[X]},Str};

tz({etag,Tag,Attrs},?m("/>",Str))         -> {text,Str,{sc,Tag,Attrs}};
tz({etag,"script",Attrs},?m(">",Str))     -> {script,Str,{open,"script",Attrs}};
tz({etag,"style",Attrs},?m(">",Str))      -> {style,Str,{open,"style",Attrs}};
tz({etag,Tag,Attrs},?m(">",Str))          -> {text,Str,{open,Tag,Attrs}};

tz({end_tag,Tag},?D(X,S))when ?ev(X)      -> {{end_tag,Tag,'>'},ws(S)};
tz({end_tag,Tag,'>'},?m(">",Str))         -> {text,Str,{close,dc(Tag)}};
tz({end_tag,Tag},?d(X,Str))               -> {{end_tag,Tag++[X]},Str};

tz({attr,"",{Tag,As}},?D(X,S))when ?ev(X) -> {{etag,dc(Tag),As},S};
tz({attr,"",{Tag,As}},?M("/>",S))         -> {{etag,dc(Tag),As},S};
tz({attr,A,{T,As}},?D(X,S))when ?ev(X)    -> {{eatt,{dc(A),T,As}},ws(S)};
tz({attr,A,{T,As}},?M("/>",S))            -> {{eatt,{dc(A),T,As}},ws(S)};
tz({attr,A,TAs},?d(X,Str))when ?ok(X)     -> {{attr,A++[X],TAs},Str};

tz({eatt,ATAs},?m("=",Str))               -> {{val,ATAs},ws(Str)};
tz({eatt,{A,T,As}},S)                     -> {{attr,"",{T,As++[{A,""}]}},ws(S)};

tz({val,ATAs},?m("'",Str))                -> {{sqval,"",ATAs},Str};%singlequoted
tz({val,ATAs},?m("\"",Str))               -> {{dqval,"",ATAs},Str};%doublequoted
tz({val,ATAs},Str)                        -> {{uqval,"",ATAs},Str};%unquoted

tz({sqval,V,{A,T,As}},?m("'",S))          -> {{attr,"",{T,As++[{A,V}]}},ws(S)};
tz({sqval,V,ATAs},?d(X,Str))              -> {{sqval,V++[X],ATAs},Str};

tz({dqval,V,{A,T,As}},?m("\"",S))         -> {{attr,"",{T,As++[{A,V}]}},ws(S)};
tz({dqval,V,ATAs},?d(X,Str))              -> {{dqval,V++[X],ATAs},Str};

tz({uqval,V,{A,T,As}},?D(X,S))when ?ev(X) -> {{attr,"",{T,As++[{A,V}]}},ws(S)};
tz({uqval,V,{A,T,As}},?M("/>",S))         -> {{attr,"",{T,As++[{A,V}]}},ws(S)};
tz({uqval,V,ATAs},?d(X,Str))              -> {{uqval,V++[X],ATAs},Str};

tz(script,Str)                            -> ff(script,Str);
tz(style,Str)                             -> ff(style,Str);
tz(text,Str)                              -> ff(text,Str);

tz(X,"")                                  -> {X,"",eof}.

%% fast forward
ff(What,Str) -> ff(What,Str,0,Str).

ff(script,<<Tag:9/binary,Str/binary>>,N,Bin) when Tag=:=<<"</script>">>;Tag=:=<<"</SCRIPT>">> ->
  {Scr,_} = split_binary(Bin,N),
  {{tag,""},<<"/script>",Str/binary>>,{text,Scr}};
ff(style,<<Tag:8/binary,Str/binary>>,N,Bin) when Tag=:=<<"</style>">>;Tag=:=<<"</STYLE>">> ->
  {Scr,_} = split_binary(Bin,N),
  {{tag,""},<<"/style>",Str/binary>>,{text,Scr}};
ff(text,<<"<",Str/binary>>,N,Bin) ->
  {Scr,_} = split_binary(Bin,N),
  {{tag,""},ws(Str),{text,Scr}};
ff(_,<<>>,_,Str) ->
  {{text,Str},"",eof};
ff(What,<<_,Str/binary>>,N,Bin) ->
  ff(What,Str,N+1,Bin).

ws(?d(X,Str)) when ?ws(X) -> ws(Str);
ws(Str) -> Str.

dc(Str) -> string:to_lower(Str).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Parsing real pages
wget_parse(Url) ->
  wget_sax(Url,fun(T,A)-> A++[T] end,[]).

wget_sax(Url,Fun,A0) ->
  sax(wget(Url),Fun,A0).

wget_print(Url) ->
  io:fwrite("~s~n",[wget(Url)]).

wget(Url) ->
  inets:start(),
  {ok, {_Rc, _Hdrs, Body}} = httpc:request(get, {Url, []}, [], []),
  Body.
