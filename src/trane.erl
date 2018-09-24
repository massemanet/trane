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
%% get page from web server

wget(Url) ->
  inets:start(),
  {ok, {_Rc, _Hdrs, Body}} = httpc:request(get, {Url, []}, [], []),
  Body.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% parser

-define(is_str(S), is_integer(hd(S))).
-record(state,
        {fn,
         acc,
         subject,
         stack=[],
         res = mk_regexps()}).

parse(Str, Fun, Acc) when ?is_str(Str) ->
  parse(list_to_binary(Str), Fun, Acc);
parse(Str, Fun, Acc) when is_binary(Str) ->
  State = #state{fn=Fun, acc=Acc, subject=Str},
  eof(parse_loop(State, tokenize(State, 0))).

parse_loop(State0 = #state{stack=Stack}, {[Token], Ix}) ->
  case Ix of
    eof ->
      maybe_emit(Token, unroll(Stack, State0#state{stack=[]}));
    _   ->
      State = maybe_emit(Token, State0),
      parse_loop(State, tokenize(State, Ix))
  end;
parse_loop(State, {[Token|Tokens], Tail}) ->
  parse_loop(maybe_emit(Token, State), {Tokens, Tail}).

eof(#state{acc=Acc}) -> Acc.

maybe_emit(Token, State) ->
  case Token of
    {text, <<>>}       -> State;
    {oc, Tag, Attrs}   -> emit({end_tag, Tag}, emit({tag, Tag, Attrs}, State));
    {co, Tag, Attrs}   -> emit({tag, Tag, Attrs}, emit({end_tag, Tag}, State));
    {open, Tag, Attrs} -> emit({tag, Tag, Attrs}, push(Tag, State));
    {close, Tag, _}    -> emit_end_tag(Tag, State);
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

-define(DQ, "(?:\"(?:[^\\\"]|\\\")*\")"). %% double quoted string
-define(SQ, "(?:'(?:[^\']|\')*')").       %% single quoted string
-define(UQ, "(?:\\w+)").                  %% unquoted string

-define(COMMENT,
        <<"<!--(.*)-->">>).  %% comment content (mandatory, maybe empty)
-define(TAG_START,
        <<"<(/)?\\s*(",?UQ,")">>).  %% closing tag marker (opt), tag name
-define(TAG_ATTR,
        <<"\\G\\s+(",?UQ,")",  %% attr name (mandatory)
          "(?:\\s*=\\s*(",?SQ,"|",?DQ,"|",?UQ,"))?">>). %% attr value (opt)
-define(TAG_END,
        <<"\\G\s*(\/)?>">>).  %% self-closing tag marker (opt)

mk_regexps() ->
  fun(tag_start) -> element(2, re:compile(?TAG_START, [caseless]));
     (tag_attr)  -> element(2, re:compile(?TAG_ATTR, [caseless]));
     (tag_end)   -> element(2, re:compile(?TAG_END, [caseless]))
  end.

tokenize(State, Ix0) ->
  case tag_begin(State, Ix0) of
    nomatch ->
      Tail = snip(State, Ix0),
      {[{text, Tail}], eof};
    {match, Ix1, Endp, Tag} ->
      case tag_end(State, Ix1) of
        {match, Ix2, Attrs, SelfClosep} ->
          Pre = snip(State, Ix0, Ix1-Ix0),
          Token = {type(Endp, SelfClosep), Tag, Attrs},
          {[{text, Pre}, Token], Ix2};
        nomatch ->
          tokenize(State, Ix0+1)
      end
  end.

type(Endp, SelfClosep) ->
  case {Endp, SelfClosep} of
    {false, false} -> open; %% <a>   = open
    {true, false} -> close; %% </a>  = close
    {false, true} -> oc;    %% <a/>  = open, close
    {true, true} -> co      %% </a/> = close, open
  end.

tag_begin(State, Ix) ->
  case re_run(State, tag_start, Ix) of
    nomatch ->
      nomatch;
    {match, [Endp, {Pos, Len}]} ->
      {match, Pos+Len, endp(Endp), snip(State, Pos, Len)}
  end.

endp(Endp) -> {-1, 0} =:= Endp.

tag_end(State, Ix0) ->
  {Ix1, Attrs} = tag_attrs(State, {Ix0, []}),
  case re_run(State, tag_end, Ix1) of
    nomatch -> nomatch;
    {match, []} -> {match, Ix1+1, Attrs, false};
    {match, [_]} -> {match, Ix1+2, Attrs, true}
  end.

tag_attrs(State, {Ix, O}) ->
  case re_run(State, tag_attr, Ix) of
    nomatch ->
      {Ix, lists:reverse(O)};
    {match, [{Pos, Len}]} ->
      tag_attrs(State, {Pos+Len, [{snip(State, Pos, Len), <<>>}|O]});
    {match, [{P0, L0}, {P1, L1}]} ->
      tag_attrs(State, {P1+L1, [{snip(State, P0, L0), snip(State, P1, L1)}|O]})
  end.

snip(State, Ix) ->
  snip(State, Ix, byte_size(State#state.subject)-Ix).

snip(#state{subject=Bin}, Pos, Len) ->
  binary:part(Bin, Pos, Len).

re_run(#state{subject=Subj, res=REs}, Tag, Ix) ->
  re:run(Subj, REs(Tag), [{capture, all_but_first}, {offset, Ix}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% tokenizer
%%
%% tokenize(text, Str, R) ->
%%   case ff(text, Str, R) of
%%     {{text, Str}, eof} ->
%%       {[{text, Str}], eof};
%%     {{text, Text}, Tail} ->
%%       %% we've found a '<'. try to treat it as a tag, if that fails, s/</&lt;/
%%       try
%%         {Token, TailTail} = tz({tag, ""}, Tail, R),
%%         {[{text, Text}, Token], TailTail}
%%       catch
%%         _:_ -> tokenize(text, <<Text/binary, "&lt;", Tail/binary>>, R)
%%       end
%%   end;
%% tokenize(script, Str, R) ->
%%   case ff(script, Str, R) of
%%     {{script, Text}, eof} -> {[{text, Text}], eof};
%%     {Token, Tail} -> {[Token, {close, "script"}], Tail}
%%   end;
%% tokenize(style, Str, R) ->
%%   case ff(style, Str, R) of
%%     {{style, Text}, eof} -> {[{text, Text}], eof};
%%     {Token, Tail} -> {[Token, {close, "style"}], Tail}
%%   end.
%%
%% `tz' tokenizes tags
%% operators
%% -define(m(X, B), <<X, B/binary>>).               % consume - match
%% -define(d(X, B), <<X:8/integer, B/binary>>).     % consume - decompose
%% -define(M(X, B), <<X, _/binary>> = B).           % peek - match
%% -define(D(X, B), <<X:8/integer, _/binary>> = B). % peek - decompose

%% %% tests
%% -define(ok(X), $a=<X, X=<$z;$A=<X, X=<$Z;$0=<X, X=<$9;X==$_;X==$-;X==$:).
%% -define(ws(X), X==$\s;X==$\r;X==$\n;X==$\t).
%% -define(dq(X), X==$").
%% -define(sq(X), X==$').
%% -define(ev(X), ?ws(X);X==$>;X==$=).

%% %% we have found a '<', now in tag context
%% tz({tag, ""}, ?m("!--", _), _)          -> 'comment';
%% tz({tag, ""}, ?m("!", Str), R)          -> '!';
%% tz({tag, ""}, ?m("?", Str), R)          -> '?';
%% tz({tag, ""}, ?m("/", Str), R)            -> tz({end_tag, ""}, ws(Str), R);
%% tz({tag, Tag}, ?m("/>", Str), _)          -> {{self_closed, Tag, []}, Str};
%% tz({tag, Tag}, ?D(X, S), R) when ?ev(X)   -> tz({attr, "", Tag, []}, ws(S), R);
%% tz({tag, Tag}, ?d(X, Str), R) when ?ok(X) -> tz({tag, Tag++[X]}, Str, R);

%% %% we've found '<!'
%% tz({'!', DT}, ?m(">", Str), _) -> {{'!', dc(DT)}, Str};
%% tz({'!', DT}, ?d(X, Str), R)   -> tz({'!', DT++[X]}, Str, R);

%% %% we've found '<?'
%% tz({'?', DT}, ?m("?>", Str), _) -> {{'?', dc(DT)}, Str};
%% tz({'?', DT}, ?d(X, Str), R)    -> tz({'?', DT++[X]}, Str, R);

%% %% we found '</', in end_tag context
%% tz({end_tag, Tag, '>'}, ?m(">", Str), _)    -> {{close, dc(Tag)}, Str};
%% tz({end_tag, Tag}, ?D(X, S), R) when ?ev(X) -> tz({end_tag, Tag, '>'}, ws(S), R);
%% tz({end_tag, Tag}, ?d(X, Str), R)           -> tz({end_tag, Tag++[X]}, Str, R);

%% %% in attribute context
%% tz({attr, "", T, As}, ?D(X, S), R) when ?ev(X) -> tz({etag, dc(T), As}, S, R);
%% tz({attr, "", T, As}, ?M("/>", S), R)          -> tz({etag, dc(T), As}, S, R);
%% tz({attr, A, T, As}, ?D(X, S), R) when ?ev(X)  -> tz({eatt, dc(A), T, As}, ws(S), R);
%% tz({attr, A, T, As}, ?M("/>", S), R)           -> tz({eatt, dc(A), T, As}, ws(S), R);
%% tz({attr, A, T, As}, ?d(X, S), R) when ?ok(X)  -> tz({attr, A++[X], T, As}, S, R);

%% %% we were in 'tag' and there should be a '>' here
%% tz({etag, Tag, Attrs}, ?m("/>", Str), _) -> {{sc, Tag, Attrs}, Str};
%% tz({etag, Tag, Attrs}, ?m(">", Str), _)  -> {{open, Tag, Attrs}, Str};

%% %% was in 'attribute' context, found end of attribute
%% tz({eatt, A, T, As}, ?m("=", Str), R) -> tz({val, A, T, As}, ws(Str), R);
%% tz({eatt, A, T, As}, S, R)            -> tz({attr, "", T, As++[{A, ""}]}, ws(S), R);

%% %% found an attribute that has a value
%% tz({val, A, T, As}, ?m("'", S), R)  -> tz({sqval, "", A, T, As}, S, R); %singlequoted
%% tz({val, A, T, As}, ?m("\"", S), R) -> tz({dqval, "", A, T, As}, S, R); %doublequoted
%% tz({val, A, T, As}, S, R)           -> tz({uqval, "", A, T, As}, S, R); %unquoted

%% %% in single quoted value context
%% tz({sqval, V, A, T, As}, ?m("'", S), R) -> tz({attr, "", T, As++[{A, V}]}, ws(S), R);
%% tz({sqval, V, A, T, As}, ?d(X, Str), R) -> tz({sqval, V++[X], A, T, As}, Str, R);

%% %% in double quoted value context
%% tz({dqval, V, A, T, As}, ?m("\"", S), R) -> tz({attr, "", T, As++[{A, V}]}, ws(S), R);
%% tz({dqval, V, A, T, As}, ?d(X, Str), R)  -> tz({dqval, V++[X], A, T, As}, Str, R);

%% %% in unquoted value context
%% tz({uqval, V, A, T, As}, ?D(X, S), R) when ?ev(X) -> tz({attr, "", T, As++[{A, V}]}, ws(S), R);
%% tz({uqval, V, A, T, As}, ?M("/>", S), R)          -> tz({attr, "", T, As++[{A, V}]}, ws(S), R);
%% tz({uqval, V, A, T, As}, ?d(X, Str), R)           -> tz({uqval, V++[X], A, T, As}, Str, R).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% %% fast forward

%% %% regexps
%% end_text() -> "<".
%% end_script() -> "</script\\s*>".
%% end_style() -> "</style\\s*>".
%% end_comment() -> "-->".

%% %% pre-compiled versions of the regexps
%% mk_regexps() ->
%%   fun(text)   -> element(2, re:compile(end_text(), [caseless]));
%%      (script) -> element(2, re:compile(end_script(), [caseless]));
%%      (style)  -> element(2, re:compile(end_style(), [caseless]));
%%      (comment)-> element(2, re:compile(end_comment(), [caseless]));
%%      (tag_start) -> element(2, re:compile(?TAG_START, [caseless]));
%%      (tag_attr) -> element(2, re:compile(?TAG_ATTR, [caseless]));
%%      (tag_end) -> element(2, re:compile(?TAG_END, [caseless]))
%%   end.

%% %% skip ahead until re matches. 'R' holds the pre compiled regexps
%% ff(What, String, R) ->
%%   case re:split(String, R(What), [{parts, 2}]) of
%%     [Scr, Str] -> mangle(What, Scr, Str);
%%     _ -> {{What, String}, eof}
%%   end.

%% %% the 'tz' return values
%% mangle(text, Scr, Str)    -> {{text, Scr}, ws(Str)};
%% mangle(script, Scr, Str)  -> {{script, Scr}, Str};
%% mangle(style, Scr, Str)   -> {{style, Scr}, Str};
%% mangle(comment, Scr, Str) -> {{comment, Scr}, Str}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% utils

%% trim whitespace from the left
%% ws(?d(X, Str)) when ?ws(X) -> ws(Str);
%% ws(Str) -> Str.

%% downcase
%% dc(Str) -> string:to_lower(Str).
