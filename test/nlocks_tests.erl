-module(nlocks_tests).

-include_lib("eunit/include/eunit.hrl").

-spec test() -> term().

-spec nlocks_test_() -> term().
nlocks_test_() ->
    {foreach,
     fun() ->
             error_logger:tty(false)
     end,
     fun(_) ->
             error_logger:tty(true)
     end,
     [
      {<<"Basic locking">>, fun basic_locking/0},
      {<<"Basic transactions">>, fun basic_transactions/0},
      {<<"Mutual exclusion over resource">>, fun mutual_exclusion/0},
      {<<"Lost reference deallocation">>, fun lost_reference_deallocation/0},
      {<<"Regular use deallocation">>, fun regular_use_deallocation/0},
      {<<"Exception exit deallocation">>, fun exception_deallocation/0},
      {<<"Brutal kill deallocation">>, fun brutal_kill_deallocation/0}
     ]
    }.

basic_locking() ->
    Runs = 100,
    Lock = nlocks:new(),
    Fun = fun () ->
                  {ok, Ownership} = nlocks:acquire_ownership(Lock, infinity),
                  {error, timeout} = nlocks:acquire_ownership(Lock, 5),
                  ok = nlocks:release_ownership(Ownership),
                  {error, already_released} = nlocks:release_ownership(Ownership),
                  ok
          end,

    ?assertEqual(times(Runs, ok), execute(Runs, Fun)),
    ?assertEqual(times(Runs, ok), execute_parallel(Runs, Fun)).

basic_transactions() ->
    Runs = 100,
    Lock = nlocks:new(),
    TrxFun = fun () -> timer:sleep(5), done end,
    Fun = fun () ->
                  nlocks:transaction(Lock, TrxFun, infinity)
          end,

    ?assertEqual(times(Runs, {ok, done}), execute(Runs, Fun)),
    ?assertEqual(times(Runs, {ok, done}), execute_parallel(Runs, Fun)).

mutual_exclusion() ->
    Runs = 500,
    Lock = nlocks:new(),
    Table = ets:new(table, [public, {read_concurrency, true}, {write_concurrency, true}]),
    ets:insert(Table, {counter, 0}),
    TrxFun =
        fun () ->
                [{counter, Value}] = ets:lookup(Table, counter),
                timer:sleep(crypto:rand_uniform(0, 5)),
                ets:insert(Table, {counter, Value + 1})
        end,
    Fun =
        fun () ->
                nlocks:transaction(Lock, TrxFun, infinity)
        end,

    ?assertEqual(times(Runs, {ok, true}), execute_parallel(Runs, Fun)),
    ?assertEqual([{counter, Runs}], ets:lookup(Table, counter)).

lost_reference_deallocation() ->
    SharedFun =
        fun (_Lock) ->
            nothing
        end,
    IndependentFun =
        fun () ->
                SharedFun(nlocks:new())
        end,
    deallocation(10000, IndependentFun, SharedFun, nothing).

regular_use_deallocation() ->
    SharedFun =
        fun (Lock) ->
                nlocks:transaction(Lock, fun () -> did_nothing end, infinity)
        end,
    IndependentFun =
        fun () ->
                SharedFun(nlocks:new())
        end,
    deallocation(10000, IndependentFun, SharedFun, {ok, did_nothing}).

exception_deallocation() ->
    SharedFun =
        fun (Lock) ->
            catch nlocks:transaction(Lock, fun() -> exit('oh noes') end, infinity)
        end,
    IndependentFun =
        fun () ->
                SharedFun(nlocks:new())
        end,
    deallocation(10000, IndependentFun, SharedFun, {'EXIT', 'oh noes'}).

brutal_kill_deallocation() ->
    SharedFun =
        fun (Lock) ->
            nlocks:transaction(Lock,
                               fun() ->
                                       Self = self(),
                                       spawn(
                                         fun () ->
                                                 exit(Self, kill)
                                         end),
                                       receive Nothing -> Nothing end
                               end,
                               infinity)
        end,
    IndependentFun =
        fun () ->
                SharedFun(nlocks:new())
        end,
    deallocation(1000, IndependentFun, SharedFun).

deallocation(Runs, IndependentFun, SharedFun) ->
    deallocation(Runs, IndependentFun, SharedFun, undefined).

deallocation(Runs, IndependentFun, SharedFun, MaybeExpectedResult) ->
    InitialLocksCount = allocated_locks_count(),
    InitialOwnershipsCount = allocated_ownerships_count(),

    if MaybeExpectedResult =:= undefined ->
           execute_parallel_async(Runs, IndependentFun);
       true ->
           ?assertEqual(times(Runs, MaybeExpectedResult), execute_parallel(Runs, IndependentFun))
    end,
    ?assertEqual(InitialLocksCount, allocated_locks_count()),
    ?assertEqual(InitialOwnershipsCount, allocated_ownerships_count()),

    SharedLock = nlocks:new(),
    SharedFunWrapper = fun () -> SharedFun(SharedLock) end,
    if MaybeExpectedResult =:= undefined ->
           execute_parallel_async(Runs, SharedFunWrapper);
       true ->
           ?assertEqual(times(Runs, MaybeExpectedResult), execute_parallel(Runs, SharedFunWrapper))
    end,
    ?assertEqual(InitialLocksCount + 1, allocated_locks_count()),
    ?assertEqual(InitialOwnershipsCount, allocated_ownerships_count()),
    ?assertNotEqual(SharedLock, undefined). % force object to last until here without deallocation

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

execute_parallel_async(Times, Function) ->
    Parent = self(),
    ProcFunction = fun () -> Parent ! Function() end,
    Pids = execute(Times, fun () -> spawn(ProcFunction) end),
    (fun Wait() ->
             lists:any(fun erlang:is_process_alive/1, Pids) andalso Wait()
     end)().

allocated_locks_count() ->
    {allocated_locks, Value} = lists:keyfind(allocated_locks, 1, nlocks:info()),
    Value.

allocated_ownerships_count() ->
    {allocated_ownerships, Value} = lists:keyfind(allocated_ownerships, 1, nlocks:info()),
    Value.
