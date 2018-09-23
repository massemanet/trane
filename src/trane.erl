%%% -*- mode: erlang; erlang-indent-level: 2 -*-
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
-export([sax/1, sax/3, wget_parse/1, wget_print/1, wget_sax/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% API

wget_parse(Url) ->
  wget_sax(Url, fun(T, A)-> A++[T] end, []).

wget_sax(Url, Fun, A0) ->
  sax(wget(Url), Fun, A0).

sax(Str) ->
  lists:reverse(parse(Str, fun(A, B) -> [A|B] end, [])).

sax(Str, Fun, Acc) ->
  parse(Str, Fun, Acc).

wget_print(Url) ->
  io:fwrite("~s~n", [wget(Url)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% parser

-define(is_str(S), is_integer(hd(S))).
-record(state, {fn, acc, stack=[], res}).

parse(Str, Fun, Acc) when ?is_str(Str) ->
  parse(list_to_binary(Str), Fun, Acc);
parse(Str, Fun, Acc) when is_binary(Str) ->
  R = mk_regexps(),
  eof(parse_loop(#state{fn=Fun, acc=Acc, res=R}, tokenize(text, Str, R))).

parse_loop(State0 = #state{stack=Stack}, {[Token], Tail}) ->
  case Tail of
    eof ->
      maybe_emit(Token, unroll(Stack, State0#state{stack=[]}));
    _   ->
      State = maybe_emit(Token, State0),
      parse_loop(State, tokenize(next_type(Token), Tail, State#state.res))
  end;
parse_loop(State, {[Token|Tokens], Tail}) ->
  parse_loop(maybe_emit(Token, State), {Tokens, Tail}).

next_type({open, "script", _}) -> script;
next_type({open, "style", _}) -> style;
next_type(_) -> text.

eof(#state{acc=Acc}) -> Acc.

maybe_emit(Token, State) ->
  case Token of
    {text, <<>>}       -> State;
    {sc, Tag, Attrs}   -> emit({end_tag, Tag}, emit({tag, Tag, Attrs}, State));
    {open, Tag, Attrs} -> emit({tag, Tag, Attrs}, push(Tag, State));
    {close, Tag}       -> emit_end_tag(Tag, State);
    {comment, Comment} -> emit({comment, Comment}, State);
    {'?', DT}          -> emit({'?', DT}, State);
    {'!', DT}          -> emit({'!', DT}, State);
    {script, Script}   -> emit({script, Script}, State);
    {style, Style}     -> emit({style, Style}, State);
    {text, Text}       -> emit({text, Text}, State)
  end.

emit_end_tag(Tag, State) ->
  try emit({end_tag, Tag}, pop(Tag, maybe_unroll(Tag, State)))
  catch close_without_open -> State
  end.

emit(Token, State = #state{fn=Fun, acc=Acc}) ->
  State#state{acc=Fun(Token, Acc)}.

push(Tag, State = #state{stack=Stack}) ->
  State#state{stack = [Tag|Stack]}.

pop(Tag, State = #state{stack=[Tag|Stack]}) ->
  State#state{stack = Stack}.

unroll(Stack, State) ->
  lists:foldl(fun(Item, S) -> emit({end_tag, Item}, S) end, State, Stack).

maybe_unroll(Tag, State = #state{stack=Stack}) ->
  case lists:member(Tag, Stack) of
    true ->
      %% close all open tags in this tags scope
      {Hd, Tl} = lists:splitwith(fun(T)-> T=/=Tag end, Stack),
      unroll(Hd, State#state{stack=Tl});
    false->
      %% close tag has no open tag; drop it
      throw(close_without_open)
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% tokenizer

%% operators
-define(m(X, B), <<X, B/binary>>).               % consume - match
-define(d(X, B), <<X:8/integer, B/binary>>).     % consume - decompose
-define(M(X, B), <<X, _/binary>> = B).           % peek - match
-define(D(X, B), <<X:8/integer, _/binary>> = B). % peek - decompose

%% tests
-define(ok(X), $a=<X, X=<$z;$A=<X, X=<$Z;$0=<X, X=<$9;X==$_;X==$-;X==$:).
-define(ws(X), X==$\s;X==$\r;X==$\n;X==$\t).
-define(dq(X), X==$").
-define(sq(X), X==$').
-define(ev(X), ?ws(X);X==$>;X==$=).

tokenize(text, Str, R) ->
  case ff(text, Str, R) of
    {{text, Str}, eof} ->
      {[{text, Str}], eof};
    {{text, Text}, Tail} ->
      %% we've found a '<'. try to treat it as a tag, if that fails, s/</&lt;/
      try
        {Token, TailTail} = tz({tag, ""}, Tail, R),
        {[{text, Text}, Token], TailTail}
      catch
        _:_ -> tokenize(text, <<Text/binary, "&lt;", Tail/binary>>, R)
      end
  end;
tokenize(script, Str, R) ->
  case ff(script, Str, R) of
    {{script, Text}, eof} -> {[{text, Text}], eof};
    {Token, Tail} -> {[Token, {close, "script"}], Tail}
  end;
tokenize(style, Str, R) ->
  case ff(style, Str, R) of
    {{style, Text}, eof} -> {[{text, Text}], eof};
    {Token, Tail} -> {[Token, {close, "style"}], Tail}
  end.


%% we have found a '<', now in tag context
tz({tag, ""}, ?m("!--", Str), R)          -> ff(comment, Str, R);
tz({tag, ""}, ?m("!", Str), R)            -> tz({'!', ""}, ws(Str), R);
tz({tag, ""}, ?m("?", Str), R)            -> tz({'?', ""}, ws(Str), R);
tz({tag, ""}, ?m("/", Str), R)            -> tz({end_tag, ""}, ws(Str), R);
tz({tag, Tag}, ?m("/>", Str), _)          -> {{sc, Tag, []}, Str};
tz({tag, Tag}, ?D(X, S), R) when ?ev(X)   -> tz({attr, "", Tag, []}, ws(S), R);
tz({tag, Tag}, ?d(X, Str), R) when ?ok(X) -> tz({tag, Tag++[X]}, Str, R);

%% we've found '<!'
tz({'!', DT}, ?m(">", Str), _) -> {{'!', dc(DT)}, Str};
tz({'!', DT}, ?d(X, Str), R)   -> tz({'!', DT++[X]}, Str, R);

%% we've found '<?'
tz({'?', DT}, ?m("?>", Str), _) -> {{'?', dc(DT)}, Str};
tz({'?', DT}, ?d(X, Str), R)    -> tz({'?', DT++[X]}, Str, R);

%% we found '</', in end_tag context
tz({end_tag, Tag, '>'}, ?m(">", Str), _)    -> {{close, dc(Tag)}, Str};
tz({end_tag, Tag}, ?D(X, S), R) when ?ev(X) -> tz({end_tag, Tag, '>'}, ws(S), R);
tz({end_tag, Tag}, ?d(X, Str), R)           -> tz({end_tag, Tag++[X]}, Str, R);

%% in attribute context
tz({attr, "", T, As}, ?D(X, S), R) when ?ev(X) -> tz({etag, dc(T), As}, S, R);
tz({attr, "", T, As}, ?M("/>", S), R)          -> tz({etag, dc(T), As}, S, R);
tz({attr, A, T, As}, ?D(X, S), R) when ?ev(X)  -> tz({eatt, dc(A), T, As}, ws(S), R);
tz({attr, A, T, As}, ?M("/>", S), R)           -> tz({eatt, dc(A), T, As}, ws(S), R);
tz({attr, A, T, As}, ?d(X, S), R) when ?ok(X)  -> tz({attr, A++[X], T, As}, S, R);

%% we were in 'tag' and there should be a '>' here
tz({etag, Tag, Attrs}, ?m("/>", Str), _) -> {{sc, Tag, Attrs}, Str};
tz({etag, Tag, Attrs}, ?m(">", Str), _)  -> {{open, Tag, Attrs}, Str};

%% was in 'attribute' context, found end of attribute
tz({eatt, A, T, As}, ?m("=", Str), R) -> tz({val, A, T, As}, ws(Str), R);
tz({eatt, A, T, As}, S, R)            -> tz({attr, "", T, As++[{A, ""}]}, ws(S), R);

%% found an attribute that has a value
tz({val, A, T, As}, ?m("'", S), R)  -> tz({sqval, "", A, T, As}, S, R); %singlequoted
tz({val, A, T, As}, ?m("\"", S), R) -> tz({dqval, "", A, T, As}, S, R); %doublequoted
tz({val, A, T, As}, S, R)           -> tz({uqval, "", A, T, As}, S, R); %unquoted

%% in single quoted value context
tz({sqval, V, A, T, As}, ?m("'", S), R) -> tz({attr, "", T, As++[{A, V}]}, ws(S), R);
tz({sqval, V, A, T, As}, ?d(X, Str), R) -> tz({sqval, V++[X], A, T, As}, Str, R);

%% in double quoted value context
tz({dqval, V, A, T, As}, ?m("\"", S), R) -> tz({attr, "", T, As++[{A, V}]}, ws(S), R);
tz({dqval, V, A, T, As}, ?d(X, Str), R)  -> tz({dqval, V++[X], A, T, As}, Str, R);

%% in unquoted value context
tz({uqval, V, A, T, As}, ?D(X, S), R) when ?ev(X) -> tz({attr, "", T, As++[{A, V}]}, ws(S), R);
tz({uqval, V, A, T, As}, ?M("/>", S), R)          -> tz({attr, "", T, As++[{A, V}]}, ws(S), R);
tz({uqval, V, A, T, As}, ?d(X, Str), R)           -> tz({uqval, V++[X], A, T, As}, Str, R).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% fast forward

%% regexps
end_text() -> "<".
end_script() -> "</script\\s*>".
end_style() -> "</style\\s*>".
end_comment() -> "-->".

%% pre-compiled versions of the regexps
mk_regexps() ->
  fun(text)   -> element(2, re:compile(end_text(),    [caseless]));
     (script) -> element(2, re:compile(end_script(),  [caseless]));
     (style)  -> element(2, re:compile(end_style(),   [caseless]));
     (comment)-> element(2, re:compile(end_comment(), [caseless]))
  end.

%% skip ahead until re matches. 'R' holds the pre compiled regexps
ff(What, String, R) ->
  case re:split(String, R(What), [{parts, 2}]) of
    [Scr, Str] -> mangle(What, Scr, Str);
    _ -> {{What, String}, eof}
  end.

%% the 'tz' return values
mangle(text, Scr, Str)    -> {{text, Scr}, ws(Str)};
mangle(script, Scr, Str)  -> {{script, Scr}, Str};
mangle(style, Scr, Str)   -> {{style, Scr}, Str};
mangle(comment, Scr, Str) -> {{comment, Scr}, Str}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% utils

%% trim whitespace from the left
ws(?d(X, Str)) when ?ws(X) -> ws(Str);
ws(Str) -> Str.

%% downcase
dc(Str) -> string:to_lower(Str).

%% get page from web server
wget(Url) ->
  inets:start(),
  case httpc:request(get, {Url, []}, [], [{body_format, binary}]) of
    {ok, {_Rc, _Hdrs, Body}} -> Body;
    {error, Reason} -> error({error, {Url, Reason}})
  end.

