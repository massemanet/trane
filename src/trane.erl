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
  ssl:start(),
  case httpc:request(get, {Url, []}, [], [{body_format, binary}]) of
    {ok, {_Rc, _Hdrs, Body}} -> Body;
    {error, R} -> exit({bad_request, {Url, R}})
  end.

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

emit_end_tag(Tag, State = #state{stack=Stack}) ->
  case lists:splitwith(fun(T)-> T=/=Tag end, Stack) of
    {Stack, []} -> State;    %% close tag without open tag; drop it
    {H, T} -> emit({end_tag, Tag}, pop(Tag, unroll(H, State#state{stack=T})))
  end.

emit(Token, State = #state{fn=Fun, acc=Acc}) ->
  State#state{acc=Fun(Token, Acc)}.

push(Tag, State = #state{stack=Stack}) ->
  State#state{stack = [Tag|Stack]}.

pop(Tag, State = #state{stack=[Tag|Stack]}) ->
  State#state{stack = Stack}.

unroll(Stack, State) ->
  lists:foldl(fun(Item, S) -> emit({end_tag, Item}, S) end, State, Stack).

-define(DQ, "(?:\"(?:[^\"]|\\\\\")*\")").       %% double quoted string
-define(SQ, "(?:'(?:[^\']|\\\\')*')").          %% single quoted string
-define(UQ, "(?:[^>/=\\\\\"\\\\'\\s]|/(?!>))+").%% unquoted string
-define(STRING, <<?SQ,"|",?DQ,"|",?UQ>>/binary).%% a string

-define(COMMENT,
        "<!--").            %% comment. closes with "-->"
-define(COMMENT_END,
        "-->").             %% comment. closes with "-->"
-define(DOCTYPE,
        "<!doctype").       %% the tag can contain strings
-define(DOCTYPE_END,
        ">").               %% the tag can contain strings
-define(XML,
        "<\\?xml").         %% the tag can contain attrs. closes with "?>"
-define(XML_END,
        "\\?>").            %% the tag can contain attrs. closes with "?>"
-define(TAG_START,
        "<(/?)(\\s*)(\\w+)").%% close tag marker (opt), extra ws (opt), tagname
-define(TAG_ATTR,
        <<"\\G\\s+(",?STRING,")",  %% attr name (mandatory)
          "(?:\\s*=\\s*(",?STRING,"))?">>). %% attr value (opt)
-define(TAG_END,
        <<"\\G\s*(\/)?>">>).%% self-closing tag marker (opt)
-define(TAG_BEGIN,
        <<?COMMENT,"|",?DOCTYPE,"|",?XML,"|",?TAG_START>>).

mk_regexps() ->
  fun(tag_begin)       -> re_compile(?TAG_BEGIN);
     (tag_attr)        -> re_compile(?TAG_ATTR);
     (tag_end)         -> re_compile(?TAG_END);
     (tag_end_comment) -> re_compile(?COMMENT_END);
     (tag_end_doctype) -> re_compile(?DOCTYPE_END);
     (tag_end_xml)     -> re_compile(?XML_END)
  end.

tokenize(State, Ix0) ->
  tokenize(State, Ix0, Ix0).

tokenize(State, IxInit, Ix0) ->
  case tag_begin(State, IxInit, Ix0) of
    nomatch ->
      Tail = snip(State, IxInit),
      {[{text, Tail}], eof};
    {match, Ix1, Pre, Tag} ->
      case check_scope(State, Tag) andalso tag_end(State, Ix1, Tag) of
        {match, Ix2, Token} ->
          {[{text, Pre}, Token], Ix2};
        _ ->
          tokenize(State, IxInit, Ix0+1)
      end
  end.

check_scope(#state{stack=Stack}, Current) ->
  case Stack of
    [<<"script">>|_] when Current =/= {close, no_ws, <<"script">>} -> false;
    [<<"style">>|_]  when Current =/= {close, no_ws, <<"style">>} -> false;
    _ -> true
  end.

tag_begin(State, IxInit, Ix) ->
  case re_run(State, tag_begin, Ix) of
    nomatch ->
      nomatch;
    {match, [{Pos, Len}]} ->
      Pre = snip(State, IxInit, Pos-IxInit), %% snippet in front of the match
      {match, Pos+Len, Pre, whatmatch(Len)};
    {match, [{P0, _}, Endp, WS, {Pos, Len}]} -> %% Endp is the '/' of an endtag
      Pre = snip(State, IxInit, P0-IxInit), %% snippet in front of the match
      Tag = string:lowercase(snip(State, Pos, Len)),   %% the tag
      {match, Pos+Len, Pre, {open_close(Endp), ws(WS), Tag}}
  end.

whatmatch(4) -> comment; %% "<!--"
whatmatch(5) -> '?';     %% "<?xml"
whatmatch(9) -> '!'.     %% "<!doctype"

ws({_, 0}) -> no_ws;  %% there's no whitespace between the "<" and the tag
ws({_, _}) -> ws.     %% there is whitespace between the "<" and the tag

open_close({_, 0}) -> open;  %% "<" is not followed by "/"
open_close({_, 1}) -> close. %% "<" is followed by "/"

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
tag_end(State, Ix0, {OpenClose, _, Tag}) ->
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
      tag_attrs(State, {Pos+Len, [{val(State, Pos, Len), <<>>}|O]});
    {match, [_, {P0, L0}, {P1, L1}]} ->
      tag_attrs(State, {P1+L1, [{val(State, P0, L0), val(State, P1, L1)}|O]})
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

re_compile(Str) ->
  {ok, RE} = re:compile(Str, [caseless]),
  RE.
