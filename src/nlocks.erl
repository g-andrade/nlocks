-module(nlocks).
-author('Guilherme Andrade <nlocks(at)gandrade(dot)net>').
-on_load(init/0). -ignore_xref({init, 0}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([new/0]).               -ignore_xref({new, 0}).
-export([acquire_ownership/1]). -ignore_xref({acquire_ownership, 1}).
-export([acquire_ownership/2]). -ignore_xref({acquire_ownership, 2}).
-export([release_ownership/1]). -ignore_xref({release_ownership, 1}).
-export([transaction/2]).       -ignore_xref({transaction, 2}).
-export([transaction/3]).       -ignore_xref({transaction, 3}).
-export([info/0]).              -ignore_xref({info, 0}).

%% ------------------------------------------------------------------
%% Macro Definitions
%% ------------------------------------------------------------------

-define(DEFAULT_OWNERSHIP_ACQUIRE_TIMEOUT, 5000).

%% ------------------------------------------------------------------
%% Type Definitions
%% ------------------------------------------------------------------

-type wrapped_resource(Id) :: {Id, non_neg_integer(), term()}.

-opaque lock() :: wrapped_resource('nlocks.lock').
-export_type([lock/0]).

-opaque ownership() :: wrapped_resource('nlocks.ownership').
-export_type([ownership/0]).

-type trx_fun() :: fun(() -> term()).
-export_type([trx_fun/0]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec new() -> lock().
new() ->
    erlang:nif_error(nif_library_not_loaded).

-spec acquire_ownership(Lock :: lock())
        -> {ok, ownership()} | {error, timeout}.
acquire_ownership(Lock) ->
    acquire_ownership(Lock, ?DEFAULT_OWNERSHIP_ACQUIRE_TIMEOUT).

-spec acquire_ownership(Lock :: lock(), Timeout :: timeout())
        -> {ok, ownership()} | {error, timeout}.
acquire_ownership(_Lock, _Timeout) ->
    erlang:nif_error(nif_library_not_loaded).

-spec release_ownership(Ownership :: ownership())
        -> ok | {error, not_allowed} | {error, already_released}.
release_ownership(_Ownership) ->
    erlang:nif_error(nif_library_not_loaded).

-spec transaction(Lock :: lock(), Fun :: trx_fun())
        -> {ok, FunResult :: term()} | {error, timeout}.
transaction(Lock, Fun) ->
    transaction(Lock, Fun, ?DEFAULT_OWNERSHIP_ACQUIRE_TIMEOUT).

-spec transaction(Lock :: lock(), Fun :: trx_fun(), Timeout :: timeout())
        -> {ok, FunResult :: term()} | {error, timeout}.
transaction(Lock, Fun, Timeout) ->
    ownership_transaction(Fun, acquire_ownership(Lock, Timeout)).

-spec info() -> [{allocated_locks | allocated_ownerships | acquired_locks | contention, non_neg_integer()} |
                 {has_lockfree_counters | has_lockfree_ownership, boolean()}, ...].
info() ->
    erlang:nif_error(nif_library_not_loaded).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

init() ->
    ok = erlang:load_nif("./priv/nlocks_nif", 0).

-spec ownership_transaction(Fun :: trx_fun(), {ok, ownership()} | {error, timeout})
        -> {ok, FunResult :: term()} | {error, timeout}.
ownership_transaction(Fun, {ok, Ownership}) ->
    Result = try
                 Fun()
             catch
                 Class:Reason ->
                     ok = release_ownership(Ownership),
                     erlang:raise(Class, Reason, erlang:get_stacktrace())
             end,
    ok = release_ownership(Ownership),
    {ok, Result};
ownership_transaction(_Fun, {error, timeout}) ->
    {error, timeout}.
