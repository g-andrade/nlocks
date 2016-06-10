

# nlocks #

Copyright (c) 2016 Guilherme Andrade

__Version:__ 1.0.0

__Authors:__ Guilherme Andrade ([`nlocks(at)gandrade(dot)net`](mailto:nlocks(at)gandrade(dot)net)).

`nlocks`: Native spinlocks for Erlang
---------

An experiment on Erlang native spinlocks:
* Able to guarantee exclusive access to shared resources
* Not dependent on a single process (no flooded mailbox as a bottleneck)
* Able to deal with brutally killed processes that still held unreleased locks (more below)

```erlang

Lock = nlocks:new(),
Transaction = fun() ->
    io:format("hello from ~p at ~p~n", [self(), os:timestamp()]),
    timer:sleep(100)
end,

[spawn(fun () -> nlocks:transaction(Lock, Transaction) end) || _ <- lists:seq(1, 4)].
% hello from <0.66.0> at {1465,509871,454813}
% hello from <0.69.0> at {1465,509871,555028}
% hello from <0.67.0> at {1465,509871,656021}
% hello from <0.68.0> at {1465,509871,757031}

```


### <a name="Brutal_kills_and_unreleased_locks">Brutal kills and unreleased locks</a> ###


Hackish solution. Other than setting up some sort of monitor in the Erlang land, I found no practical way to deal with these other than making use of _ownership_ objects, references to which should never leave the process under which they were created; the release therefore becomes dependent on the garbage collector calling their destructor, but this also makes it more convenient for regular kills that forget to clean up.


### <a name="Ownership_objects">Ownership objects</a> ###


```c++

struct Ownership {
    ERL_NIF_TERM pid;
    Lock* lock;
};

```

* They're created for lock acquisition in order to deal with the above problem
* They should never leave their owning process
* On successful acquisition, they link themselves to the lock and force it to live as long as they live
* They guarantee only the lock owner can release the lock
* No explicit destruction needed


### <a name="Lock_objects">Lock objects</a> ###


```c++

struct Lock {
    std::atomic<Ownership*> ownership;
};

```

* They keep an atomic pointer either set to null (free) or to the ownership object's address (locked)
* Can be shared between local processes through messages, ETS tables, etc.
* No explicit destruction needed


### <a name="The_spinning">The spinning</a> ###


* It uses C++ 11 [compare_exchange_weak](http://en.cppreference.com/w/cpp/atomic/atomic/compare_exchangek)
* For each attempt, a new NIF call is scheduled; this brings some overhead to the table but it's crucial to play nice with the VM
* If the timeout is other than 'infinity', for each attempt it compares the deadline against the current [steady_clock](http://en.cppreference.com/w/cpp/chrono/steady_clock) value; this feels excessive and might hurt performance.


### <a name="Internal_metrics">Internal metrics</a> ###


```erlang

% nlocks:info()
[{allocated_locks,2},
 {allocated_ownerships,6},
 {acquired_locks,1},
 {contention,5}, % amount of processes attempting lock acquisition
 {has_lockfree_counters,true},
 {has_lockfree_ownership,true}]

```



## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="nlocks.md" class="module">nlocks</a></td></tr></table>

