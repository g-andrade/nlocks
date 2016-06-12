-module(nvars).
-author('Guilherme Andrade <nlocks(at)gandrade(dot)net>').
-compile({no_auto_import, [{get, 0}, {put, 1}]}).

%% ------------------------------------------------------------------
%% Contextualized Function Exports
%% ------------------------------------------------------------------

-export([get/0]).     -ignore_xref({get, 0}).
-export([put/1]).     -ignore_xref({put, 1}).
-export([replace/1]). -ignore_xref({replace, 1}).

% ------------------------------------------------------------------
% API Function Exports
% ------------------------------------------------------------------

-export([new/0]).            -ignore_xref({new, 0}).
-export([new/1]).            -ignore_xref({new, 1}).
-export([load/1]).           -ignore_xref({load, 1}).
-export([load/2]).           -ignore_xref({load, 2}).
-export([store/2]).          -ignore_xref({store, 2}).
-export([store/3]).          -ignore_xref({store, 3}).
-export([exchange/2]).       -ignore_xref({exchange, 2}).
-export([exchange/3]).       -ignore_xref({exchange, 3}).
-export([execute/2]).        -ignore_xref({execute, 2}).
-export([execute/3]).        -ignore_xref({execute, 3}).
-export([dirty_load/1]).     -ignore_xref({dirty_load, 1}).
-export([dirty_store/2]).    -ignore_xref({dirty_store, 2}).
-export([dirty_exchange/2]). -ignore_xref({dirty_exchange, 2}).
-export([dirty_execute/2]).  -ignore_xref({dirty_execute, 2}).


%% ------------------------------------------------------------------
%% Macro Definitions
%% ------------------------------------------------------------------

-define(PROCDIC_CONTEXT_VAR, 'nvars.context.current_var').

%% ------------------------------------------------------------------
%% Record Definitions
%% ------------------------------------------------------------------

-record(nvar, {
          lock :: nlocks:lock(),
          var :: vegrandis:atomic_var()
         }).
-type nvar() :: #nvar{}.
-export_type([nvar/0]).

%% ------------------------------------------------------------------
%% Contextualized Function Definitions
%% ------------------------------------------------------------------

-spec get() -> Value :: term().
get() ->
    vegrandis:load(context_var()).

-spec put(Value :: term()) -> ok.
put(Value) ->
    vegrandis:store(context_var(), Value).

-spec replace(Value :: term()) -> PrevValue :: term().
replace(Value) ->
    vegrandis:exchange(context_var(), Value).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec new() -> nvar().
new() ->
    new(undefined).

-spec new(InitialValue :: term()) -> nvar().
new(InitialValue) ->
    Var = vegrandis:new(term),
    vegrandis:store(Var, InitialValue),
    #nvar{
       lock = nlocks:new(),
       var = Var
      }.

-spec load(NVar :: nvar()) -> {ok, term()} | {error, timeout}.
load(NVar) ->
    execute(NVar, fun () -> get() end).

-spec load(NVar :: nvar(), Timeout :: timeout()) -> {ok, term()} | {error, timeout}.
load(NVar, Timeout) ->
    execute(NVar, fun () -> get() end, Timeout).

-spec store(NVar :: nvar(), Value :: term()) -> ok | {error, timeout}.
store(NVar, Value) ->
    case execute(NVar, fun () -> put(Value) end) of
        {ok, ok} -> ok;
        {error, _} = Err -> Err
    end.

-spec store(NVar :: nvar(), Value :: term(), Timeout :: timeout()) -> ok | {error, timeout}.
store(NVar, Value, Timeout) ->
    case execute(NVar, fun () -> put(Value) end, Timeout) of
        {ok, ok} -> ok;
        {error, _} = Err -> Err
    end.

-spec exchange(NVar :: nvar(), Value :: term()) -> {ok, PrevValue :: term()} | {error, timeout}.
exchange(NVar, Value) ->
    execute(NVar, fun () -> replace(Value) end).

-spec exchange(NVar :: nvar(), Value :: term(), Timeout :: timeout())
        -> {ok, PrevValue :: term()} | {error, timeout}.
exchange(NVar, Value, Timeout) ->
    execute(NVar, fun () -> replace(Value) end, Timeout).

-spec execute(NVar :: nvar(), Function :: fun (() -> (Return :: term())))
        -> {ok, Return :: term()} | {error, timeout}.
execute(#nvar{ lock = Lock } = NVar, Function) ->
    nlocks:transaction(Lock, contextualize_function(NVar, Function)).

-spec execute(NVar :: nvar(), Function :: fun (() -> (Return :: term())),
              Timeout :: timeout())
        -> {ok, Return :: term()} | {error, timeout}.
execute(#nvar{ lock = Lock } = NVar, Function, Timeout) ->
    nlocks:transaction(Lock, contextualize_function(NVar, Function), Timeout).

-spec dirty_load(NVar :: nvar()) -> term().
dirty_load(NVar) ->
    dirty_execute(NVar, fun () -> get() end).

-spec dirty_store(NVar :: nvar(), Value :: term()) -> ok.
dirty_store(NVar, Value) ->
    dirty_execute(NVar, fun () -> put(Value) end).

-spec dirty_exchange(NVar :: nvar(), Value :: term()) -> PrevValue :: term().
dirty_exchange(NVar, Value) ->
    dirty_execute(NVar, fun () -> replace(Value) end).

-spec dirty_execute(NVar :: nvar(), Function :: fun (() -> (Return :: term())))
        -> Return :: term().
dirty_execute(NVar, Function) ->
    (contextualize_function(NVar, Function))().

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

-spec contextualize_function(nvar(), fun (() -> term())) -> fun (() -> term()).
contextualize_function(#nvar{ var = Var }, Function) ->
    fun () ->
            PrevVar = put(?PROCDIC_CONTEXT_VAR, Var),
            Result = try
                         Function()
                     catch
                         Class:Reason ->
                             put(?PROCDIC_CONTEXT_VAR, PrevVar),
                             erlang:raise(Class, Reason, erlang:get_stacktrace())
                     end,
            put(?PROCDIC_CONTEXT_VAR, PrevVar),
            Result
    end.

-spec context_var() -> vegrandis:atomic_var() | no_return().
context_var() ->
    case get(?PROCDIC_CONTEXT_VAR) of
        undefined -> exit(cannot_execute_outside_context);
        Var -> Var
    end.
