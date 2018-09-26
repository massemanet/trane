%% -*- mode: erlang; erlang-indent-level: 2 -*-
%% @doc
%% @end

-module(trane_eunit).

-include_lib("eunit/include/eunit.hrl").

t_style_test_() ->
  {ok, X} = file:read_file("../test/style-tag.html"),
  Lines = re:split(X, "\n"),

  [?_assertEqual(
      [{'?',<<"xml version=\"1.0\" encoding=\"utf-8\"">>}],
      t_sax(lists:nth(1, Lines))),
   ?_assertEqual(
      [{comment,<<" renders: \"0 1 2 3 4\" ">>}],
      t_sax(lists:nth(2, Lines))),
   ?_assertEqual(
      [],
      t_sax(lists:nth(3, Lines))),
   ?_assertEqual(
      [{text,<<"0">>},
       {tag,<<"style">>,[{<<"class">>,<<"b">>}]},
       {text,<<"<></>">>},
       {end_tag,<<"style">>},
       {text,<<" 1">>}],
      t_sax(lists:nth(4, Lines))),
   ?_assertEqual(
      [{tag,<<"script">>,[]},
       {text,<<"   foo(\"<\\/script>\"); ">>},
       {end_tag,<<"script">>},
       {text,<<"2">>}],
      t_sax(lists:nth(5, Lines))),
   ?_assertEqual(
      [{text,<<"3 ">>},
       {tag,<<"style">>,[{<<"type">>,<<"text/css">>}]},
       {text,<<"img#wpstats{display:none}">>},
       {end_tag,<<"style">>},
       {text,<<"4">>}],
      t_sax(lists:nth(6, Lines))),
   ?_assertEqual(
      [{tag,<<"style">>,[]},
       {end_tag,<<"style">>}],
      t_sax(lists:nth(7, Lines))),
   ?_assertEqual(
      [{tag,<<"style">>,[]},
       {end_tag,<<"style">>},
       {text,<<"style>">>}],
      t_sax(lists:nth(8, Lines)))].

t_script_test_() ->
  {ok, X} = file:read_file("../test/script-tag.html"),
  Lines = re:split(X, "\n"),

  [?_assertEqual(
      [{comment,<<" renders: \"0 1 2 3 4 5 6 7 8\" ">>}],
      t_sax(lists:nth(1, Lines))),
   ?_assertEqual(
      [],
      t_sax(lists:nth(2, Lines))),
   ?_assertEqual(
      [{tag,<<"script">>,[{<<"type">>,<<"/text/javascript">>}]},
       {text,<<"foo(\"<\\\\/script>\");">>},
       {end_tag,<<"script">>},
       {text,<<"0">>}],
      t_sax(lists:nth(3, Lines))),
   ?_assertEqual(
      [{tag,<<"script">>,[]},
       {text,<<"   foo(\"<\\/script>\"); ">>},
       {end_tag,<<"script">>},
       {text,<<"1">>}],
      t_sax(lists:nth(4, Lines))),
   ?_assertEqual(
      [{tag,<<"script">>,[]},
       {end_tag,<<"script">>},
       {text,<<" foo('</ script>');< /SCRIPT>1.5">>}],
      t_sax(lists:nth(5, Lines))),
   ?_assertEqual(
      [{tag,<<"script">>,[]},
       {text,<<" foo('< /script>');">>},
       {end_tag,<<"script">>},
       {text,<<"2">>}],
      t_sax(lists:nth(6, Lines))),
   ?_assertEqual(
      [{tag,<<"script">>,[]},
       {end_tag,<<"script">>},
       {text,<<" foo(\"</ script>\");</scrip>2.5">>}],
      t_sax(lists:nth(7, Lines))),
   ?_assertEqual(
      [{tag,<<"script">>,[]},
       {text,<<" foo(\"</ script>\");">>},
       {end_tag,<<"script">>},
       {text,<<"3">>}],
      t_sax(lists:nth(8, Lines))),
   ?_assertEqual(
      [{tag,<<"script">>,[]},
       {end_tag,<<"script">>},
       {text,<<" foo(\"</ script>\");</ script>3.5">>}],
      t_sax(lists:nth(9, Lines))),
   ?_assertEqual(
      [{tag,<<"script">>,[]},
       {text,<<" foo(\"">>},
       {end_tag,<<"script">>},
       {text,<<"4">>}],
      t_sax(lists:nth(10, Lines))),
   ?_assertEqual(
      [{tag,<<"script">>,[]},
       {text,<<" foo(\"">>},
       {end_tag,<<"script">>},
       {text,<<"5">>}],
      t_sax(lists:nth(11, Lines))),
   ?_assertEqual(
      [{tag,<<"script">>,[]},
       {end_tag,<<"script">>},
       {text,<<" foo(\"</ script>\");</ script>5.5">>}],
      t_sax(lists:nth(12, Lines))),
   ?_assertEqual(
      [{tag,<<"script">>,[]},
       {text,<<" foo(\"">>},
       {end_tag,<<"script">>},
       {text,<<"6">>}],
      t_sax(lists:nth(13, Lines))),
   ?_assertEqual(
      [{tag,<<"script">>,[]},
       {text,<<" foo <script> foo(\"">>},
       {end_tag,<<"script">>},
       {text,<<"7">>}],
      t_sax(lists:nth(14, Lines))),
   ?_assertEqual(
      [{tag,<<"script">>,[]},
       {text,<<" foo(\"<\\\\/script>\")">>},
       {end_tag,<<"script">>},
       {text,<<"8">>}],
      t_sax(lists:nth(15, Lines)))].

t_basic_test_() ->
  {ok, X} = file:read_file("../test/basic.html"),
  Lines = re:split(X, "\n"),

  [?_assertEqual(
      [{'!',<<"doctype bla">>},
       {tag,<<"p">>,[{<<"a">>,<<"b">>},{<<"c">>,<<"d">>}]},
       {end_tag,<<"p">>}],
      t_sax(lists:nth(1, Lines))),
   ?_assertEqual(
      [{tag,<<"head">>,[]},
       {tag,<<"b">>,[]},
       {tag,<<"p">>,[]},
       {tag,<<"p">>,[]},
       {end_tag,<<"p">>},
       {end_tag,<<"p">>},
       {end_tag,<<"b">>},
       {end_tag,<<"head">>}],
      t_sax(lists:nth(2, Lines))),
   ?_assertEqual(
      [{tag,<<"tag">>,[{<<"catt">>,<<>>},{<<"xatt">>,<<>>},{<<"batt">>,<<>>}]},
       {end_tag,<<"tag">>}],
      t_sax(lists:nth(3, Lines))),
   ?_assertEqual(
      [{tag,<<"a">>,[{<<"href">>,<<"/a/bc/d.e">>}]},
       {text,<<"x">>},
       {end_tag,<<"a">>}],
      t_sax(lists:nth(4, Lines))),
   ?_assertEqual(
      [{tag,<<"a">>,[]},
       {text,<<"...<...">>},
       {end_tag,<<"a">>}],
      t_sax(lists:nth(5, Lines))),
   ?_assertEqual(
      lists:foldl(fun(_X,[_X|Y])-> Y;(_X,_Y)->exit({_X,_Y}) end,
      [{tag,<<"p">>,[{<<"a">>,<<"b">>},{<<"c">>,<<"d">>}]},
       {text,<<"hej">>},
       {comment,<<" te ">>},
       {tag,<<"b">>,[]},
       {text,<<"svejs">>},
       {end_tag,<<"b">>},
       {text,<<"foo">>},
       {end_tag,<<"p">>},
       {text,<<"grmp">>},
       {tag,<<"br">>,[]},
       {end_tag,<<"br">>},
       {tag,<<"x">>,[{<<"x">>,<<"y">>}]},
       {end_tag,<<"x">>}],
      t_sax(lists:nth(6, Lines))),[]),
   ?_assertEqual(
      [{tag,<<"script">>,[]},
       {text,<<"visual+basic = rules;">>},
       {end_tag,<<"script">>}],
      t_sax(lists:nth(7, Lines))),
   ?_assertEqual(
      [{tag,<<"script">>,[]},
       {text,<<"visual+basic = rules;">>},
       {end_tag,<<"script">>}],
      t_sax(lists:nth(8, Lines))),
   ?_assertEqual(
      [{tag,<<"script">>,[]},
       {text,<<"visual+basic = \"rules<\\/script>\";">>},
       {end_tag,<<"script">>}],
      t_sax(lists:nth(9, Lines))),
   ?_assertEqual(
      [{text,<<"<\\">>}],
      t_sax(lists:nth(10, Lines))),
   ?_assertEqual(
      [{text,<<"head">>},
       {tag,<<"a">>,[{<<"href">>,<<"foo">>}]},
       {end_tag,<<"a">>},
       {text,<<"tail">>}],
      t_sax(lists:nth(11, Lines))),
   ?_assertEqual(
      [{tag,<<"p">>,[{<<"bla">>,<<>>},{<<"baz">>,<<>>}]},
       {end_tag,<<"p">>},
       {text,<<"c">>}],
      t_sax(lists:nth(12, Lines))),
   ?_assertEqual(
      [{tag,<<"div">>,[{<<"bla">>,<<"baz">>}]},
       {end_tag,<<"div">>},
       {text,<<"d">>}],
      t_sax(lists:nth(13, Lines)))].

t_sax(Str) ->
  trane:sax(Str,fun(T,A)-> A++[T] end,[]).
