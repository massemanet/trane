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
-export([sax/3, wget_parse/1, wget_print/1, wget_sax/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% API

wget_parse(Url) ->
  wget_sax(Url, fun(T, A)-> A++[T] end, []).

wget_sax(Url, Fun, A0) ->
  sax(wget(Url), Fun, A0).

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
  parse_loop(#state{fn=Fun, acc=Acc, res=R}, tokenize({text, Str, []}, R)).

parse_loop(State, [{Token, T}]) ->
  case Token of
    eof -> erlang:display({trailing,T}),
           NS = maybe_emit(Token, State),
           eof(unroll(NS#state.stack, NS#state{stack=[]}));
    _   -> parse_loop(maybe_emit(Token, State), tokenize(T, State#state.res))
  end;
parse_loop(State, [Token|Ts]) ->
  parse_loop(maybe_emit(Token, State), Ts).

eof(#state{acc=Acc}) -> Acc.

maybe_emit(Token, State) ->
  case Token of
    {sc, Tag, Attrs}  -> emit({end_tag, Tag}, emit({tag, Tag, Attrs}, State));
    {open, Tag, Attrs}-> emit({tag, Tag, Attrs}, push(Tag, State));
    {close, Tag}      -> emit_end_tag(Tag, State);
    {text, <<>>}      -> State;
    eof               -> State;
    {'?', DT}         -> emit({'?', DT}, State);
    {comment, Comment}-> emit({comment, Comment}, State);
    {'!', DT}         -> emit({'!', DT}, State);
    {script, Script}  -> emit({script, Script}, State);
    {style, Style}    -> emit({style, Style}, State);
    {text, Text}      -> emit({text, Text}, State)
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

unroll(X, State) ->
  lists:foldl(fun(T, S) -> emit({end_tag, T}, S) end, State, X).

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

tokenize({TZ, Str}, R) ->
  tokenize(tz(TZ, Str, R), R);
tokenize({text, Str, Token}, R) ->
  case ff(text, Str, R) of
    {{tag, ""}, T, {text, Txt}} ->
      %% we've found a '<'. try to treat it as a tag, if that fails, s/</&lt;/
      try lists:flatten([Token, {text, Txt}]++tokenize({{tag, ""}, T}, R))
      catch _:_ -> tokenize({text, <<Txt/binary, "&lt;", T/binary>>, Token}, R)
      end;
    {{text, Str}, "", eof} ->
      lists:flatten([Token, {eof, {text, Str}}])
  end;
tokenize({TZ, Str, Token}, _) ->
  [{Token, {TZ, Str}}].

%% we found a '<', now in tag context
tz({tag, ""}, ?m("!--", Str), R)         -> ff(comment, Str, R);
tz({tag, ""}, ?m("!", Str), _)           -> {{'!', ""}, ws(Str)};
tz({tag, ""}, ?m("?", Str), _)           -> {{que, ""}, ws(Str)};
tz({tag, ""}, ?m("/", Str), _)           -> {{end_tag, ""}, ws(Str)};
tz({tag, Tag}, ?m("/>", Str), _)         -> {text, Str, {sc, Tag, []}};
tz({tag, Tag}, ?D(X, S), _) when ?ev(X)   -> {{attr, "", {Tag, []}}, ws(S)};
tz({tag, Tag}, ?d(X, Str), _) when ?ok(X) -> {{tag, Tag++[X]}, Str};

%% we've found '<!'
tz({'!', DT}, ?m(">", Str), _) -> {text, Str, {'!', dc(DT)}};
tz({'!', DT}, ?d(X, Str), _)   -> {{'!', DT++[X]}, Str};

%% we've found '<?'
tz({'?', DT}, ?m("?>", Str), _) -> {text, Str, {'?', dc(DT)}};
tz({'?', DT}, ?d(X, Str), _)    -> {{'?', DT++[X]}, Str};

%% we were in 'tag' and found a '>'
tz({etag, Tag, Attrs}, ?m("/>", Str), _)     -> {text, Str, {sc, Tag, Attrs}};
tz({etag, "script", Attrs}, ?m(">", Str), _) -> {script, Str, {open, "script", Attrs}};
tz({etag, "style", Attrs}, ?m(">", Str), _)  -> {style, Str, {open, "style", Attrs}};
tz({etag, Tag, Attrs}, ?m(">", Str), _)      -> {text, Str, {open, Tag, Attrs}};

%% we found '</', in end_tag context
tz({end_tag, Tag}, ?D(X, S), _) when ?ev(X) -> {{end_tag, Tag, '>'}, ws(S)};
tz({end_tag, Tag, '>'}, ?m(">", Str), _)   -> {text, Str, {close, dc(Tag)}};
tz({end_tag, Tag}, ?d(X, Str), _)          -> {{end_tag, Tag++[X]}, Str};

%% in attribute context
tz({attr, "", {Tag, As}}, ?D(X, S), _) when ?ev(X) -> {{etag, dc(Tag), As}, S};
tz({attr, "", {Tag, As}}, ?M("/>", S), _)         -> {{etag, dc(Tag), As}, S};
tz({attr, A, {T, As}}, ?D(X, S), _) when ?ev(X)    -> {{eatt, {dc(A), T, As}}, ws(S)};
tz({attr, A, {T, As}}, ?M("/>", S), _)            -> {{eatt, {dc(A), T, As}}, ws(S)};
tz({attr, A, TAs}, ?d(X, Str), _) when ?ok(X)      -> {{attr, A++[X], TAs}, Str};

%% was in 'attribute' context, found end of attribute
tz({eatt, ATAs}, ?m("=", Str), _) -> {{val, ATAs}, ws(Str)};
tz({eatt, {A, T, As}}, S, _)      -> {{attr, "", {T, As++[{A, ""}]}}, ws(S)};

%% found an attribute that ahs a value
tz({val, ATAs}, ?m("'", Str), _)  -> {{sqval, "", ATAs}, Str}; %singlequoted
tz({val, ATAs}, ?m("\"", Str), _) -> {{dqval, "", ATAs}, Str}; %doublequoted
tz({val, ATAs}, Str, _)           -> {{uqval, "", ATAs}, Str}; %unquoted

%% in single quoted value context
tz({sqval, V, {A, T, As}}, ?m("'", S), _) -> {{attr, "", {T, As++[{A, V}]}}, ws(S)};
tz({sqval, V, ATAs}, ?d(X, Str), _)       -> {{sqval, V++[X], ATAs}, Str};

%% in double quoted value context
tz({dqval, V, {A, T, As}}, ?m("\"", S), _) -> {{attr, "", {T, As++[{A, V}]}}, ws(S)};
tz({dqval, V, ATAs}, ?d(X, Str), _)        -> {{dqval, V++[X], ATAs}, Str};

%% in unquoted value context
tz({uqval, V, {A, T, As}}, ?D(X, S), _) when ?ev(X) -> {{attr, "", {T, As++[{A, V}]}}, ws(S)};
tz({uqval, V, {A, T, As}}, ?M("/>", S), _)          -> {{attr, "", {T, As++[{A, V}]}}, ws(S)};
tz({uqval, V, ATAs}, ?d(X, Str), _)                 -> {{uqval, V++[X], ATAs}, Str};

%% fast-forward past non-html stuff
tz(script, Str, R) -> ff(script, Str, R);
tz(style, Str, R)  -> ff(style, Str, R);
tz(text, Str, R)   -> ff(text, Str, R);

%% end of string
tz(X, "", _) -> {X, "", eof}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% fast forward

%% regexps
end_text() -> "<".
end_script() -> "</script\\s*>".
end_style() -> "</style\\s*>".
end_comment() -> "-->".

%% pre-compiled versions of the regexps
mk_regexps() ->
  fun(text)   -> element(2, re:compile(end_text(), [caseless]));
     (script) -> element(2, re:compile(end_script(), [caseless]));
     (style)  -> element(2, re:compile(end_style(), [caseless]));
     (comment)-> element(2, re:compile(end_comment(), [caseless]))
  end.

%% skip ahead until re matches. 'R' holds the pre compiled regexps
ff(What, String, R) ->
  case re:split(String, R(What), [{parts, 2}]) of
    [Scr, Str] -> mangle(What, Scr, Str);
    _ -> {{text, String}, "", eof}
  end.

%% the 'tz' return values
mangle(text, Scr, Str)    -> {{tag, ""}, ws(Str), {text, Scr}};
mangle(script, Scr, Str)  -> {{tag, ""}, <<"/script>", Str/binary>>, {script, Scr}};
mangle(style, Scr, Str)   -> {{tag, ""}, <<"/style>", Str/binary>>, {style, Scr}};
mangle(comment, Scr, Str) -> {text, Str, {comment, Scr}}.

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
  {ok, {_Rc, _Hdrs, Body}} = httpc:request(get, {Url, []}, [], []),
  Body.
