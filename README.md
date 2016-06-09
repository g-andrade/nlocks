

# nlocks #

Copyright (c) 2016 Guilherme Andrade

__Version:__ 1.0.0

__Authors:__ Guilherme Andrade ([`nlocks(at)gandrade(dot)net`](mailto:nlocks(at)gandrade(dot)net)).

`nlocks`: Native spinlocks for Erlang

---------

`nlocks` provides native spinlocks that can be shared between Erlang local processes.

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



## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="https://github.com/g-andrade/nlocks/blob/master/doc/nlocks.md" class="module">nlocks</a></td></tr></table>

