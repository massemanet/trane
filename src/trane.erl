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

-define(DQ, "(?:\"(?:[^\\\"]|\\\")*\")").  %% double quoted string
-define(SQ, "(?:'(?:[^\']|\')*')").        %% single quoted string
-define(UQ, "(?:[^>\\\"\'\\s/]|/(?!>))+"). %% unquoted string

-define(COMMENT,
        "<!--").            %% comment. closes with "-->"
-define(COMMENT_END,
        "-->").             %% comment. closes with "-->"
-define(DOCTYPE,
        "<!doctype").       %% the tag can contain strings
-define(DOCTYPE_END,
        ">").               %% the tag can contain strings
-define(XML,
        "<?xml").           %% the tag can contain attrs. closes with "?>"
-define(XML_END,
        "?>").              %% the tag can contain attrs. closes with "?>"
-define(TAG_START,
        "<(/)?\\s*(\\w+)"). %% closing tag marker (opt), tag name
-define(TAG_ATTR,
        <<"\\G\\s+(\\w+)",  %% attr name (mandatory)
          "(?:\\s*=\\s*(",?SQ,"|",?DQ,"|",?UQ,"))?">>). %% attr value (opt)
-define(TAG_END,
        <<"\\G\s*(\/)?>">>).    %% self-closing tag marker (opt)
-define(TAG_BEGIN,
        <<?COMMENT,"|",?DOCTYPE,"|",?XML,"|",?TAG_START>>).

mk_regexps() ->
  fun(tag_begin)       -> element(2, re:compile(?TAG_BEGIN, [caseless]));
     (tag_attr)        -> element(2, re:compile(?TAG_ATTR, [caseless]));
     (tag_end)         -> element(2, re:compile(?TAG_END, [caseless]));
     (tag_end_comment) -> element(2, re:compile(?COMMENT_END, [caseless]));
     (tag_end_doctype) -> element(2, re:compile(?DOCTYPE_END, [caseless]));
     (tag_end_xml)     -> element(2, re:compile(?XML_END, [caseless]))
  end.

tokenize(State, Ix0) ->
  case tag_begin(State, Ix0) of
    nomatch ->
      Tail = snip(State, Ix0),
      {[{text, Tail}], eof};
    {match, Ix1, Pre, Tag} ->
      case tag_end(State, Ix1, Tag) of
        {match, Ix2, Token} ->
          {[{text, Pre}, Token], Ix2};
        nomatch ->
          tokenize(State, Ix0+1)
      end
  end.

tag_begin(State, Ix) ->
  case re_run(State, tag_begin, Ix) of
    nomatch ->
      nomatch;
    {match, [{Pos, 4}]} ->  %% comment
      Pre = snip(State, Ix, Pos-Ix),    %% the snippet in front of the match
      {match, Pos+4, Pre, comment};
    {match, [{Pos, 9}]} ->  %% doctype
      Pre = snip(State, Ix, Pos-Ix),    %% the snippet in front of the match
      {match, Pos+9, Pre, '!'};
    {match, [{Pos, 5}]} ->  %% xml
      Pre = snip(State, Ix, Pos-Ix),    %% the snippet in front of the match
      {match, Pos+5, Pre, '?'};
    {match, [{P0, _}, Endp, {Pos, Len}]} ->
      Pre = snip(State, Ix, P0-Ix),    %% the snippet in front of the match
      Tag = string:lowercase(snip(State, Pos, Len)),   %% the tag
      {match, Pos+Len, Pre, {open_close(Endp), Tag}}
  end.

open_close({-1, 0}) -> open;
open_close(_) -> close.

-define(END_NORM, [_]).
-define(END_SELF, [_,_]).

tag_end(State, Ix0, '?') ->
  {Ix1, Attrs} = tag_attrs(State, {Ix0, []}),
  case re_run(State, tag_end_xml, Ix1) of
    nomatch -> nomatch;
    {match, [{Pos, Len}]} -> {match, Pos+Len, {'?', Attrs}}
  end;
tag_end(State, Ix0, '!') ->
  {Ix1, Attrs} = tag_attrs(State, {Ix0, []}),
  case re_run(State, tag_end_doctype, Ix1) of
    nomatch -> nomatch;
    {match, [{Pos, Len}]} -> {match, Pos+Len, {'!', Attrs}}
  end;
tag_end(State, Ix0, comment) ->
  case re_run(State, tag_end_comment, Ix0) of
    nomatch -> {match, eof, {comment, snip(State, Ix0)}};
    {match, [{Pos, 3}]} -> {match, Pos+3, {comment, snip(State, Ix0, Pos-Ix0)}}
  end;
tag_end(State, Ix0, {OpenClose, Tag}) ->
  {Ix1, Attrs} = tag_attrs(State, {Ix0, []}),
  case re_run(State, tag_end, Ix1) of
    nomatch -> nomatch;
    {match, [{P, L}]} -> {match, P+L, {type(norm, OpenClose), Tag, Attrs}};
    {match, [{P, L}, _]} -> {match, P+L, {type(self, OpenClose), Tag, Attrs}}
  end.

type(norm, open)  -> open;  %% <a>   = open
type(norm, close) -> close; %% </a>  = close
type(self, open)  -> oc;    %% <a/>  = open, close
type(self, close) -> co.    %% </a/> = close, open

tag_attrs(State, {Ix, O}) ->
  case re_run(State, tag_attr, Ix) of
    nomatch ->
      {Ix, lists:reverse(O)};
    {match, [_, {Pos, Len}]} ->
      tag_attrs(State, {Pos+Len, [{snip(State, Pos, Len), <<>>}|O]});
    {match, [_, {P0, L0}, {P1, L1}]} ->
      tag_attrs(State, {P1+L1, [{snip(State, P0, L0), val(State, P1, L1)}|O]})
  end.

val(State, Pos, Len) ->
  Snip = snip(State, Pos, Len),
  case (-1 < (L = Len-2)) andalso Snip of
    <<"'",X:L/binary,"'">> -> X;
    <<"\"",X:L/binary,"\"">> -> X;
    _ -> Snip
  end.

snip(State, Ix) ->
  snip(State, Ix, byte_size(State#state.subject)-Ix).

snip(#state{subject=Bin}, Pos, Len) ->
  binary:part(Bin, Pos, Len).

re_run(#state{subject=Subj, res=REs}, Tag, Ix) ->
  re:run(Subj, REs(Tag), [{offset, Ix}]).
