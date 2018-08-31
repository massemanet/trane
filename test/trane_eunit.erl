%% -*- mode: erlang; erlang-indent-level: 2 -*-
%% @doc
%% @end

-module(trane_eunit).

-include_lib("eunit/include/eunit.hrl").

t_test_() ->
  [?_assertEqual(
      t_sax("<!DOCTYPE bla><P a=b c=d>"),
      [{'!',"doctype bla"},
       {tag,"p",[{"a","b"},{"c","d"}]},
       {end_tag,"p"}]),
   ?_assertEqual(
      t_sax("<head><B>< p><p ></z></ b></x>"),
      [{tag,"head",[]},
       {tag,"b",[]},
       {tag,"p",[]},
       {tag,"p",[]},
       {end_tag,"p"},
       {end_tag,"p"},
       {end_tag,"b"},
       {end_tag,"head"}]),
   ?_assertEqual(
      t_sax("<tag catt xatt=\"\" batt>"),
      [{tag,"tag",[{"catt",""},{"xatt",""},{"batt",""}]},
       {end_tag,"tag"}]),
   ?_assertEqual(
      t_sax("<a href=/a/bc/d.e>x</a>"),
      [{tag,"a",[{"href","/a/bc/d.e"}]},
       {text,<<"x">>},
       {end_tag,"a"}]),
   ?_assertEqual(
      t_sax("<a>...<...</a>"),
      [{tag,"a",[]},
       {text,<<"...&lt;...">>},
       {end_tag,"a"}]),
   ?_assertEqual(
      t_sax("<P a=b c=d>hej<!-- te --><b>svejs</b>foo</p>grmp<br/><x x=y />"),
      [{tag,"p",[{"a","b"},{"c","d"}]},
       {text,<<"hej">>},
       {comment," te "},
       {tag,"b",[]},
       {text,<<"svejs">>},
       {end_tag,"b"},
       {text,<<"foo">>},
       {end_tag,"p"},
       {text,<<"grmp">>},
       {tag,"br",[]},
       {end_tag,"br"},
       {tag,"x",[{"x","y"}]},
       {end_tag,"x"}]),
   ?_assertEqual(
      t_sax("<script>visual+basic = rules;</script>"),
      [{tag,"script",[]},
       {text,<<"visual+basic = rules;">>},
       {end_tag,"script"}]),
   ?_assertEqual(
      [{tag,"script",[]},
       {text,<<"visual+basic = rules;">>},
       {end_tag,"script"}],
      t_sax("<script>visual+basic = rules;</Script>")),
   ?_assertEqual(
      t_sax("<script>visual+basic = \"rules</script>\";</script>"),
      [{tag,"script",[]},
       {text,<<"visual+basic = \"rules</script>\";">>},
       {end_tag,"script"}])
  ].

t_sax(Str) ->
  trane:sax(Str,fun(T,A)-> A++[T] end,[]).
