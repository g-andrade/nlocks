-module(nvars_tests).

-include_lib("eunit/include/eunit.hrl").

-spec test() -> term().

-spec nvars_test_() -> term().
nvars_test_() ->
    {foreach,
     fun() ->
             error_logger:tty(false)
     end,
     fun(_) ->
             error_logger:tty(true)
     end,
     [
      {<<"nvars functionality">>, fun nvars_functionality/0}
     ]
    }.


nvars_functionality() ->
    Runs = 50000,
    InitialCounters = {0, 0}, % negative, positive
    NVar = nvars:new(InitialCounters),
    IncrementFun =
        fun (Elem, Increment) ->
                fun () ->
                        Counters = nvars:get(),
                        Value = element(Elem, Counters),
                        NewValue = Value + Increment,
                        NewCounters = setelement(Elem, Counters, NewValue),
                        nvars:put(NewCounters)
                end
        end,

    Fun = fun () ->
                  {ok, ok} = nvars:execute(NVar, IncrementFun(1, -1)),
                  {ok, ok} = nvars:execute(NVar, IncrementFun(2, +1)),
                  counters_updated
          end,

    ?assertEqual({error, timeout}, nvars:load(NVar, 0)),
    ?assertEqual({error, timeout}, nvars:load(NVar, 0)),
    ?assertEqual({error, timeout}, nvars:store(NVar, undefined, 0)),
    ?assertEqual({error, timeout}, nvars:exchange(NVar, undefined, 0)),
    ?assertEqual({ok, {0, 0}}, nvars:load(NVar, infinity)),
    ?assertEqual(times(Runs, counters_updated), execute_parallel(Runs, Fun)),
    ?assertEqual({ok, {-Runs, Runs}}, nvars:load(NVar, infinity)),

    ?assertEqual({-Runs, Runs}, nvars:dirty_load(NVar)),
    ?assertEqual(ok, nvars:dirty_execute(NVar, IncrementFun(1, -1))),
    ?assertEqual(ok, nvars:dirty_execute(NVar, IncrementFun(2, +1))),
    ?assertEqual({-Runs -1, Runs +1}, nvars:dirty_exchange(NVar, undefined)),
    ?assertEqual(undefined, nvars:dirty_load(NVar)).

times(Times, Value) when Times >= 1 ->
    [Value | times(Times - 1, Value)];
times(_Times, _Value) ->
    [].

execute(Times, Function) when Times >= 1 ->
    [Function() | execute(Times - 1, Function)];
execute(_Times, _Function) ->
    [].

execute_parallel(Times, Function) ->
    Parent = self(),
    ProcFunction = fun () -> Parent ! Function() end,
    execute(Times, fun () -> spawn_link(ProcFunction) end),
    execute(Times, fun () -> receive Res -> Res end end).
