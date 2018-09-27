%% -*- mode: erlang; erlang-indent-level: 2 -*-
%% @doc
%% @end

-module(trane_eunit).

-include_lib("eunit/include/eunit.hrl").

t_style_test_() ->
  {ok, X} = file:read_file("../test/style-tag.html"),
  Lines = re:split(X, "\n"),

  [?_assert(
      match(
        lists:nth(1, Lines),
        [{'?',[{<<"version">>,<<"1.0">>},{<<"encoding">>,<<"utf-8">>}]}])),
   ?_assert(
      match(
        lists:nth(2, Lines),
        [{comment,<<" renders: \"0 1 2 3 4\" ">>}])),
   ?_assert(
      match(
        lists:nth(3, Lines),
        [])),
   ?_assert(
      match(
        lists:nth(4, Lines),
        [{text,<<"0">>},
         {tag,<<"style">>,[{<<"class">>,<<"b">>}]},
         {text,<<"<></>">>},
         {end_tag,<<"style">>},
         {text,<<" 1">>}])),
   ?_assert(
      match(
        lists:nth(5, Lines),
        [{tag,<<"script">>,[]},
         {text,<<"   foo(\"<\\/script>\"); ">>},
         {end_tag,<<"script">>},
         {text,<<"2">>}])),
   ?_assert(
      match(
        lists:nth(6, Lines),
        [{text,<<"3 ">>},
         {tag,<<"style">>,[{<<"type">>,<<"text/css">>}]},
         {text,<<"img#wpstats{display:none}">>},
         {end_tag,<<"style">>},
         {text,<<"4">>}])),
   ?_assert(
      match(
        lists:nth(7, Lines),
        [{tag,<<"style">>,[]},
         {end_tag,<<"style">>}])),
   ?_assert(
      match(
        lists:nth(8, Lines),
        [{tag,<<"style">>,[]},
         {end_tag,<<"style">>},
         {text,<<"style>">>}]))].

t_script_test_() ->
  {ok, X} = file:read_file("../test/script-tag.html"),
  Lines = re:split(X, "\n"),

  [?_assert(
      match(
        lists:nth(1, Lines),
        [{'!',[{<<"bla=foo">>,<<>>}]},
         {tag,<<"p">>,[{<<"a">>,<<"b">>},
                       {<<"c">>,<<"d">>}]},
         {end_tag,<<"p">>}])),
   ?_assert(
      match(
        lists:nth(2, Lines),
        [{comment,<<" renders: \"0 1 2 3 4 5 6 7 8\" ">>}])),
   ?_assert(
      match(
        lists:nth(3, Lines),
        [])),
   ?_assert(
      match(
        lists:nth(4, Lines),
        [{tag,<<"script">>,[{<<"type">>,<<"/text/javascript">>}]},
         {text,<<"foo(\"<\\\\/script>\");">>},
         {end_tag,<<"script">>},
         {text,<<"0">>}])),
   ?_assert(
      match(
        lists:nth(5, Lines),
        [{tag,<<"script">>,[]},
         {text,<<"   foo(\"<\\/script>\"); ">>},
         {end_tag,<<"script">>},
         {text,<<"1">>}])),
   ?_assert(
      match(
        lists:nth(6, Lines),
        [{tag,<<"script">>,[]},
         {end_tag,<<"script">>},
         {text,<<" foo('</ script>');< /SCRIPT>1.5">>}])),
   ?_assert(
      match(
        lists:nth(7, Lines),
        [{tag,<<"script">>,[]},
         {text,<<" foo('< /script>');">>},
         {end_tag,<<"script">>},
         {text,<<"2">>}])),
   ?_assert(
      match(
        lists:nth(8, Lines),
        [{tag,<<"script">>,[]},
         {end_tag,<<"script">>},
         {text,<<" foo(\"</ script>\");</scrip>2.5">>}])),
   ?_assert(
      match(
        lists:nth(9, Lines),
        [{tag,<<"script">>,[]},
         {text,<<" foo(\"</ script>\");">>},
         {end_tag,<<"script">>},
         {text,<<"3">>}])),
   ?_assert(
      match(
        lists:nth(10, Lines),
        [{tag,<<"script">>,[]},
         {end_tag,<<"script">>},
         {text,<<" foo(\"</ script>\");</ script>3.5">>}])),
   ?_assert(
      match(
        lists:nth(11, Lines),
        [{tag,<<"script">>,[]},
         {text,<<" foo(\"">>},
         {end_tag,<<"script">>},
         {text,<<"4">>}])),
   ?_assert(
      match(
        lists:nth(12, Lines),
        [{tag,<<"script">>,[]},
         {text,<<" foo(\"">>},
         {end_tag,<<"script">>},
         {text,<<"5">>}])),
   ?_assert(
      match(
        lists:nth(13, Lines),
        [{tag,<<"script">>,[]},
         {end_tag,<<"script">>},
         {text,<<" foo(\"</ script>\");</ script>5.5">>}])),
   ?_assert(
      match(
        lists:nth(14, Lines),
        [{tag,<<"script">>,[]},
         {text,<<" foo(\"">>},
         {end_tag,<<"script">>},
         {text,<<"6">>}])),
   ?_assert(
      match(
        lists:nth(15, Lines),
        [{tag,<<"script">>,[]},
         {text,<<" foo <script> foo(\"">>},
         {end_tag,<<"script">>},
         {text,<<"7">>}])),
   ?_assert(
      match(
        lists:nth(16, Lines),
        [{tag,<<"script">>,[]},
         {text,<<" foo(\"<\\\\/script>\")">>},
         {end_tag,<<"script">>},
         {text,<<"8">>}]))].

t_basic_test_() ->
  {ok, X} = file:read_file("../test/basic.html"),
  Lines = re:split(X, "\n"),

  [?_assert(
      match(
        lists:nth(1, Lines),
        [{'!',[{<<"bla">>,<<>>}]},
         {tag,<<"p">>,[{<<"a">>,<<"b">>},
                       {<<"c">>,<<"d">>}]},
         {end_tag,<<"p">>}])),
   ?_assert(
      match(
        lists:nth(2, Lines),
        [{tag,<<"head">>,[]},
         {tag,<<"b">>,[]},
         {tag,<<"p">>,[]},
         {tag,<<"p">>,[]},
         {end_tag,<<"p">>},
         {end_tag,<<"p">>},
         {end_tag,<<"b">>},
         {end_tag,<<"head">>}])),
   ?_assert(
      match(
        lists:nth(3, Lines),
        [{tag,<<"tag">>,[{<<"catt">>,<<>>},
                         {<<"xatt">>,<<>>},
                         {<<"batt">>,<<>>}]},
         {end_tag,<<"tag">>}])),
   ?_assert(
      match(
        lists:nth(4, Lines),
        [{tag,<<"a">>,[{<<"href">>,<<"/a/bc/d.e">>}]},
         {text,<<"x">>},
         {end_tag,<<"a">>}])),
   ?_assert(
      match(
        lists:nth(5, Lines),
        [{tag,<<"a">>,[]},
         {text,<<"...<...">>},
         {end_tag,<<"a">>}])),
   ?_assert(
      match(lists:nth(6, Lines),
            [{tag,<<"p">>,[{<<"a">>,<<"b">>},
                           {<<"c">>,<<"d">>}]},
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
             {end_tag,<<"x">>}])),
   ?_assert(
      match(
        lists:nth(7, Lines),
        [{tag,<<"script">>,[]},
         {text,<<"visual+basic = rules;">>},
         {end_tag,<<"script">>}])),
   ?_assert(
      match(
        lists:nth(8, Lines),
        [{tag,<<"script">>,[]},
         {text,<<"visual+basic = rules;">>},
         {end_tag,<<"script">>}])),
   ?_assert(
      match(
        lists:nth(9, Lines),
        [{tag,<<"script">>,[]},
         {text,<<"visual+basic = \"rules<\\/script>\";">>},
         {end_tag,<<"script">>}])),
   ?_assert(
      match(
        lists:nth(10, Lines),
        [{text,<<"<\\">>}])),
   ?_assert(
      match(
        lists:nth(11, Lines),
        [{text,<<"head">>},
         {tag,<<"a">>,[{<<"href">>,<<"foo">>}]},
         {end_tag,<<"a">>},
         {text,<<"tail">>}])),
   ?_assert(
      match(
        lists:nth(12, Lines),
        [{tag,<<"p">>,[{<<"bla">>,<<>>},
                       {<<"baz">>,<<>>}]},
         {end_tag,<<"p">>},
         {text,<<"c">>}])),
   ?_assert(
      match(
        lists:nth(13, Lines),
        [{tag,<<"div">>,[{<<"bla">>,<<"baz">>}]},
         {end_tag,<<"div">>},
         {text,<<"d">>}])),
   ?_assert(
      match(
        lists:nth(14, Lines),
        [{text,<<"a">>},
         {tag,<<"a">>,[{<<"href">>,<<"baz">>}]},
         {text,<<"b">>},
         {end_tag,<<"a">>},
         {text,<<"c">>}])),
   ?_assert(
      match(
        lists:nth(15, Lines),
        [{tag,<<"a">>,[]},
         {end_tag,<<"a">>}])),
   ?_assert(
      match(
        lists:nth(16, Lines),
        [{text,<<">">>},
         {tag,<<"a">>,[]},
         {end_tag,<<"a">>}])),
   ?_assert(
      match(
        lists:nth(17, Lines),
        [{text,<<"<">>},
         {tag,<<"a">>,[]},
         {end_tag,<<"a">>}])),
   ?_assert(
      match(
        lists:nth(18, Lines),
        [{tag,<<"a">>,[]},
         {text,<<">">>},
         {end_tag,<<"a">>}])),
   ?_assert(
      match(
        lists:nth(19, Lines),
        [{tag,<<"a">>,[]},
         {text,<<"<">>},
         {end_tag,<<"a">>}])),
   ?_assert(
      match(
        lists:nth(20, Lines),
        [{tag,<<"a">>,[]},
         {end_tag,<<"a">>},
         {text,<<">">>}])),
   ?_assert(
      match(
        lists:nth(21, Lines),
        [{tag,<<"a">>,[]},
         {end_tag,<<"a">>},
         {text,<<"<">>}])),
   ?_assert(
      match(
        lists:nth(22, Lines),
        [{tag,<<"a">>,[{<<"b">>,<<">">>}]},
         {end_tag,<<"a">>}])),
   ?_assert(
      match(
        lists:nth(23, Lines),
        [{tag,<<"a">>,[{<<"b">>,<<"<">>}]},
         {end_tag,<<"a">>}])),
   ?_assert(
      match(
        lists:nth(24, Lines),
        [{text,<<">">>}])),
   ?_assert(
      match(
        lists:nth(25, Lines),
        [{text,<<"<">>}])),
   ?_assert(
      match(
        lists:nth(26, Lines),
        [{tag,<<"h3">>,[]},
         {text,<<"aaa">>},
         {end_tag,<<"h3">>}])),
   ?_assert(
      match(
        lists:nth(27, Lines),
        [{tag,<<"a">>,[{<<"href">>,<<"i">>}]},
         {text,<<"a  ">>},
         {end_tag,<<"a">>},
         {tag,<<"br">>,[]},
         {end_tag,<<"br">>}])),
   ?_assert(
      match(
        lists:nth(28, Lines),
        [{tag,<<"a">>,[{<<"href">>,<<"i">>}]},
         {text,<<"b  ">>},
         {end_tag,<<"a">>},
         {tag,<<"br">>,[]},
         {end_tag,<<"br">>}])),
   ?_assert(
      match(
        lists:nth(29, Lines),
        [{tag,<<"a">>,[{<<"href">>,<<"i">>}]},
         {text,<<"c < /a>">>},
         {end_tag,<<"a">>},
         {tag,<<"br">>,[]},
         {end_tag,<<"br">>}])),
   ?_assert(
      match(
        lists:nth(30, Lines),
        [{tag,<<"a">>,[{<<"href">>,<<"i">>}]},
         {text,<<"d < /a>">>},
         {end_tag,<<"a">>},
         {tag,<<"br">>,[]},
         {end_tag,<<"br">>}])),
   ?_assert(
      match(
        lists:nth(31, Lines),
        [{tag,<<"a">>,[{<<"href">>,<<"i">>}]},
         {text,<<"e ">>},
         {end_tag,<<"a">>},
         {tag,<<"br">>,[]},
         {end_tag,<<"br">>}])),
   ?_assert(
      match(
        lists:nth(32, Lines),
        [{tag,<<"a">>,[{<<"href">>,<<"i">>}]},
         {text,<<"f ">>},
         {end_tag,<<"a">>},
         {tag,<<"br">>,[]},
         {end_tag,<<"br">>}])),
   ?_assert(
      match(
        lists:nth(33, Lines),
        [{tag,<<"a">>,[{<<"href">>,<<"i">>}]},
         {text,<<"g< / a>">>},
         {end_tag,<<"a">>},
         {tag,<<"br">>,[]},
         {end_tag,<<"br">>}])),
   ?_assert(
      match(
        lists:nth(34, Lines),
        [{tag,<<"a">>,[{<<"href">>,<<"i">>}]},
         {text,<<"h< / a>">>},
         {end_tag,<<"a">>},
         {tag,<<"br">>,[]},
         {end_tag,<<"br">>}])),
   ?_assert(
      match(
        lists:nth(35, Lines),
        [{tag,<<"script">>,[]},
         {end_tag,<<"script">>},
         {text,<<"iii">>},
         {tag,<<"br">>,[]},
         {end_tag,<<"br">>}])),
   ?_assert(
      match(
        lists:nth(36, Lines),
        [{tag,<<"script">>,[]},
         {text,<<"< /script  >iji">>},
         {end_tag,<<"script">>},
         {tag,<<"br">>,[]},
         {end_tag,<<"br">>}])),
   ?_assert(
      match(
        lists:nth(37, Lines),
        [{tag,<<"script">>,[]},
         {text,<<"</ script  >iki">>},
         {end_tag,<<"script">>},
         {tag,<<"br">>,[]},
         {end_tag,<<"br">>}])),
   ?_assert(
      match(
        lists:nth(38, Lines),
        [{tag,<<"script">>,[]},
         {text,<<"< / script >ili">>},
         {end_tag,<<"script">>},
         {tag,<<"br">>,[]},
         {end_tag,<<"br">>}])),
   ?_assert(
      match(
        lists:nth(39, Lines),
        [{tag,<<"br">>,[]},
         {end_tag,<<"br">>}])),
   ?_assert(
      match(
        lists:nth(40, Lines),
        [{tag,<<"style">>,[]},
         {end_tag,<<"style">>},
         {text,<<"jii">>},
         {tag,<<"br">>,[]},
         {end_tag,<<"br">>}])),
   ?_assert(
      match(lists:nth(41, Lines),
            [{tag,<<"style">>,[]},
             {text,<<"< /style  >jji">>},
             {end_tag,<<"style">>},
             {tag,<<"br">>,[]},
             {end_tag,<<"br">>}])),
   ?_assert(
      match(
        lists:nth(42, Lines),
        [{tag,<<"style">>,[]},
         {text,<<"</ style  >jki">>},
         {end_tag,<<"style">>},
         {tag,<<"br">>,[]},
         {end_tag,<<"br">>}])),
   ?_assert(
      match(
        lists:nth(43, Lines),
        [{tag,<<"style">>,[]},
         {text,<<"< / style >jli">>},
         {end_tag,<<"style">>},
         {tag,<<"br">>,[]},
         {end_tag,<<"br">>}])),
   ?_assert(
      match(
        lists:nth(44, Lines),
        [{tag,<<"br">>,[]},
         {end_tag,<<"br">>}])),
   ?_assert(
      match(
        lists:nth(45, Lines),
        [{text,<<"um ">>},
         {tag,<<"a">>,[{<<"href">>,<<"/li>">>}]},
         {text,<<"fe">>},
         {end_tag,<<"a">>},
         {text,<<" sd ">>},
         {tag,<<"a">>,[{<<"href">>,<<"/wo">>}]},
         {text,<<"Wo">>},
         {end_tag,<<"a">>},
         {text,<<" q">>},
         {tag,<<"a">>,[{<<"href">>,<<"/ab">>}]},
         {text,<<"a">>},
         {end_tag,<<"a">>},
         {text,<<"o.">>},
         {tag,<<"a">>,[]},
         {end_tag,<<"a">>}])),
   ?_assert(
      match(
        lists:nth(46, Lines),
        [{tag,<<"a">>,[{<<"attr">>,<<>>}]},
         {text,<<" ">>},
         {tag,<<"a">>,
          [{<<"attr">>,<<>>},
           {<<"foo">>,<<"aa">>},
           {<<"bla">>,<<"ba-z">>}]},
         {text,<<" ">>},
         {tag,<<"p">>,[]},
         {end_tag,<<"p">>},
         {end_tag,<<"a">>},
         {end_tag,<<"a">>}])),
   ?_assert(
      match(
        lists:nth(47, Lines),
        [{tag,<<"a">>,[{<<"href">>,<<"foo">>}]},
         {text,<<"l">>},
         {tag,<<"a">>,[]},
         {end_tag,<<"a">>},
         {end_tag,<<"a">>}])),
   ?_assert(
      match(
        lists:nth(48, Lines),
        [{tag,<<"a">>,[{<<"href">>,<<"foo">>}]},
         {text,<<"l">>},
         {end_tag,<<"a">>},
         {tag,<<"a">>,[]},
         {end_tag,<<"a">>},
         {text,<<"b">>}])),
   ?_assert(
      match(
        lists:nth(49, Lines),
        [{text,<<"m">>},
         {tag,<<"p">>,[]},
         {end_tag,<<"p">>},
         {text,<<"n">>},
         {text,<<"o">>},
         {tag,<<"p">>,[]},
         {end_tag,<<"p">>},
         {text,<<"p">>}])),
   ?_assert(
      match(
        lists:nth(50, Lines),
        [])),
   ?_assert(
      match(
        lists:nth(51, Lines),
        [])),
   ?_assert(
      match(
        lists:nth(52, Lines),
        [])),
   ?_assert(
      match(
        lists:nth(53, Lines),
        [])),
   ?_assert(
      match(
        lists:nth(54, Lines),
        [])),
   ?_assert(
      match(
        lists:nth(55, Lines),
        [])),
   ?_assert(
      match(
        lists:nth(56, Lines),
        [])),
   ?_assert(
      match(
        lists:nth(57, Lines),
        [])),
   ?_assert(
      match(
        lists:nth(58, Lines),
        [])),
   ?_assert(
      match(
        lists:nth(59, Lines),
        []))
  ].

match(Line, Match) ->
  Pop = fun(X, [X|Y]) -> Y;
           (X, [Y|R]) -> throw({{wanted, Y}, {got, X}, {rest, R}});
           (X, []) -> throw({{wanted_nothing}, {got, X}})
        end,
  try trane:sax(Line, Pop, Match) of
      [] -> true;
      L -> {leftover, L}
  catch throw:Where -> Where
  end.



