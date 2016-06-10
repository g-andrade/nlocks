/* The MIT License (MIT)

   Copyright (c) 2016 Guilherme Andrade <nlocks(at)gandrade(dot)net>

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights
   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in all
   copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE.

   */

#include "erl_nif.h"
#include <cassert>
#include <cstring>
#include <atomic>
#include <chrono>
#include <limits>

static ERL_NIF_TERM MakeNifBoolean(ErlNifEnv* env, bool flag) {
    return (flag ? enif_make_atom(env, "true") : enif_make_atom(env, "false"));
}

static ERL_NIF_TERM WrapSuccess(ErlNifEnv* env, ERL_NIF_TERM term) {
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), term);
}

static ERL_NIF_TERM WrapError(ErlNifEnv* env, ERL_NIF_TERM term) {
    return enif_make_tuple2(env, enif_make_atom(env, "error"), term);
}

static ERL_NIF_TERM WrapError(ErlNifEnv* env, const char* error) {
    return WrapError(env, enif_make_atom(env, error));
}

static ERL_NIF_TERM WrapResourceTerm(
        ErlNifEnv* env, const char* resourceTypeId, ERL_NIF_TERM resourceTerm)
{
    return enif_make_tuple3(env,
            enif_make_atom(env, resourceTypeId),
            enif_make_uint64(env, static_cast<uint64_t>(resourceTerm)),
            resourceTerm);
}

static bool UnwrapResourceTerm(
        ErlNifEnv* env, ERL_NIF_TERM wrappedResource, ERL_NIF_TERM* resourceTerm)
{
    int tupleArity = 0;
    const ERL_NIF_TERM *tuple = nullptr;
    if (not enif_get_tuple(env, wrappedResource, &tupleArity, &tuple))
        return false;
    if (tupleArity != 3)
        return false;
    *resourceTerm = tuple[2];
    return true;
}

static inline uint64_t CurrentTimeMilliseconds() {
    std::chrono::steady_clock::time_point now = std::chrono::steady_clock::now();
    return std::chrono::duration_cast<std::chrono::milliseconds>(
            now.time_since_epoch()).count();
}


struct Ownership;

struct Lock {
    std::atomic<Ownership*> ownership;
};

struct Ownership {
    ERL_NIF_TERM pid;
    Lock* lock = nullptr;
};

#define LOCK_RESOURCE "nlocks.lock"
#define OWNERSHIP_RESOURCE "nlocks.ownership"

static ErlNifResourceType* lockResourceType = nullptr;
static ErlNifResourceType* ownershipResourceType = nullptr;

static std::atomic<uint64_t> allocatedLocksCount;
static std::atomic<uint64_t> allocatedOwnershipsCount;
static std::atomic<uint64_t> acquiredLocksCount;
static std::atomic<uint64_t> contentionCount;

static void DeleteLockResource(ErlNifEnv* /*env*/, void* resource) {
    Lock* lock = static_cast<Lock*>(resource);
    assert(lock->ownership == nullptr);
    allocatedLocksCount--;
}

static void DeleteOwnershipResource(ErlNifEnv* /*env*/, void* resource) {
    Ownership* ownership = static_cast<Ownership*>(resource);
    if (ownership->lock != nullptr) {
        // process was probably brutally killed; we never released the lock
        assert(ownership->lock->ownership == ownership);
        ownership->lock->ownership.store(nullptr);
        enif_release_resource(ownership->lock);
        ownership->lock = nullptr;
        acquiredLocksCount--;
    }
    allocatedOwnershipsCount--;
}

static int load(ErlNifEnv* env, void** /*priv*/, ERL_NIF_TERM /*load_info*/) {
    lockResourceType = enif_open_resource_type(
            env, nullptr,
            LOCK_RESOURCE,
            DeleteLockResource,
            ERL_NIF_RT_CREATE, nullptr);
    ownershipResourceType = enif_open_resource_type(
            env, nullptr,
            OWNERSHIP_RESOURCE,
            DeleteOwnershipResource,
            ERL_NIF_RT_CREATE, nullptr);
    allocatedLocksCount.store(0);
    allocatedOwnershipsCount.store(0);
    acquiredLocksCount.store(0);
    contentionCount.store(0);
    return 0;
}

int upgrade(ErlNifEnv* /*env*/, void** /*priv_data*/, void** /*old_priv_data*/, ERL_NIF_TERM /*load_info*/) {
    return 0;
}

//static void unload(ErlNifEnv* /*env*/, void* /*priv_data*/) {
//    // TODO
//}

/****************************************************************/
static ERL_NIF_TERM NewLock(ErlNifEnv* env, int /*argc*/, const ERL_NIF_TERM[] /*argv*/) {
    Lock* lock = static_cast<Lock*>(enif_alloc_resource(lockResourceType, sizeof(Lock)));
    memset(lock, 0, sizeof(Lock));
    ERL_NIF_TERM term = WrapResourceTerm(env, LOCK_RESOURCE, enif_make_resource(env, lock));
    enif_release_resource(lock);
    allocatedLocksCount++;
    return term;
}

// not exported, used internally
static ERL_NIF_TERM AcquireOwnershipRecursive(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    Lock* lock = nullptr;
    Ownership* ownership = nullptr;
    uint64_t deadline = 0;
    Ownership* currentOwnership = nullptr;

    assert(enif_get_resource(env, argv[0], lockResourceType, reinterpret_cast<void**>(&lock)));
    assert(enif_get_resource(env, argv[1], ownershipResourceType, reinterpret_cast<void**>(&ownership)));
    assert(enif_get_uint64(env, argv[2], &deadline));

    if ((deadline > 0) && (CurrentTimeMilliseconds() >= deadline)) {
        contentionCount--;
        enif_release_resource(ownership);
        return WrapError(env, "timeout");
    }

    currentOwnership = lock->ownership.load(std::memory_order_relaxed);
    if ((currentOwnership == nullptr)
            && lock->ownership.compare_exchange_weak(
                currentOwnership, ownership,
                std::memory_order_release,
                std::memory_order_relaxed))
    {
        // locked by us
        contentionCount--;
        acquiredLocksCount++;
        ErlNifPid selfPid;
        enif_self(env, &selfPid);
        ownership->pid = enif_make_pid(env, &selfPid);
        ownership->lock = lock;
        enif_keep_resource(lock);
        enif_release_resource(ownership);
        ERL_NIF_TERM wrappedOwnershipTerm = WrapResourceTerm(env, OWNERSHIP_RESOURCE, argv[1]);
        return WrapSuccess(env, wrappedOwnershipTerm);
    }
    // locked by someone else
    return enif_schedule_nif(
            env, "AcquireOwnershipRecursive", 0,
            AcquireOwnershipRecursive, argc, argv);
}

static ERL_NIF_TERM AcquireOwnership(ErlNifEnv* env, int /*argc*/, const ERL_NIF_TERM argv[]) {
    ERL_NIF_TERM lockTerm;
    Lock* lock = nullptr;
    unsigned timeout = 0;
    uint64_t deadline = 0;

    if (not UnwrapResourceTerm(env, argv[0], &lockTerm))
        return enif_make_badarg(env);
    if (not enif_get_resource(env, lockTerm, lockResourceType, reinterpret_cast<void**>(&lock)))
        return enif_make_badarg(env);

    if (not enif_get_uint(env, argv[1], &timeout)) {
        if (not enif_is_identical(argv[1], enif_make_atom(env, "infinity")))
            return enif_make_badarg(env);
    }
    else {
        uint64_t currentTime = CurrentTimeMilliseconds();
        deadline = currentTime + static_cast<uint64_t>(timeout);
    }

    ErlNifPid self;
    enif_self(env, &self);

    Ownership* ownership = static_cast<Ownership*>(
            enif_alloc_resource(ownershipResourceType, sizeof(Ownership)));
    memset(ownership, 0, sizeof(Ownership));
    allocatedOwnershipsCount++;
    contentionCount++;

    int newArgc = 3;
    ERL_NIF_TERM* newArgv = static_cast<ERL_NIF_TERM*>(malloc(sizeof(ERL_NIF_TERM) * newArgc));
    newArgv[0] = lockTerm;
    newArgv[1] = enif_make_resource(env, ownership);
    newArgv[2] = enif_make_uint64(env, deadline);
    return enif_schedule_nif(
            env, "AcquireOwnershipRecursive", 0,
            AcquireOwnershipRecursive, newArgc, newArgv);
}

static ERL_NIF_TERM ReleaseOwnership(ErlNifEnv* env, int /*argc*/, const ERL_NIF_TERM argv[]) {
    ERL_NIF_TERM ownershipTerm;
    Ownership* ownership = nullptr;
    if (not UnwrapResourceTerm(env, argv[0], &ownershipTerm))
        return enif_make_badarg(env);
    if (not enif_get_resource(env, ownershipTerm, ownershipResourceType, reinterpret_cast<void**>(&ownership)))
        return enif_make_badarg(env);

    ErlNifPid selfPid;
    enif_self(env, &selfPid);
    if (not enif_is_identical(enif_make_pid(env, &selfPid), ownership->pid))
        return WrapError(env, "not_allowed");
    else if (ownership->lock == nullptr)
        return WrapError(env, "already_released");
    else {
        Lock* lock = ownership->lock;
        assert(lock->ownership.load() == ownership);
        ownership->lock = nullptr;
        lock->ownership.store(nullptr);
        enif_release_resource(lock);
        acquiredLocksCount--;
        return enif_make_atom(env, "ok");
    }
}

static ERL_NIF_TERM GetInternalInfo(ErlNifEnv* env, int /*argc*/, const ERL_NIF_TERM[] /*argv*/) {
    decltype(Lock::ownership) ownershipExample;
    return enif_make_list(env,
            6,
            enif_make_tuple2(env,
                enif_make_atom(env, "allocated_locks"),
                enif_make_uint64(env, allocatedLocksCount.load())),
            enif_make_tuple2(env,
                enif_make_atom(env, "allocated_ownerships"),
                enif_make_uint64(env, allocatedOwnershipsCount.load())),
            enif_make_tuple2(env,
                enif_make_atom(env, "acquired_locks"),
                enif_make_uint64(env, acquiredLocksCount.load())),
            enif_make_tuple2(env,
                enif_make_atom(env, "contention"),
                enif_make_uint64(env, contentionCount.load())),
            enif_make_tuple2(env,
                enif_make_atom(env, "has_lockfree_counters"),
                MakeNifBoolean(env,
                    allocatedLocksCount.is_lock_free()
                    and allocatedOwnershipsCount.is_lock_free())),
            enif_make_tuple2(env,
                enif_make_atom(env, "has_lockfree_ownership"),
                MakeNifBoolean(env, ownershipExample.is_lock_free())));
}

static ErlNifFunc nif_funcs[] = {
    {"new", 0, NewLock, 0},
    {"acquire_ownership", 2, AcquireOwnership, 0},
    {"release_ownership", 1, ReleaseOwnership, 0},
    {"info", 0, GetInternalInfo, 0}
};

//ERL_NIF_INIT(nlocks_nif, nif_funcs, load, nullptr, upgrade, unload)
ERL_NIF_INIT(nlocks, nif_funcs, load, nullptr, upgrade, nullptr)
