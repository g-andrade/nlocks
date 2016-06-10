

# Module nlocks #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)



<a name="types"></a>

## Data Types ##




### <a name="type-lock">lock()</a> ###


__abstract datatype__: `lock()`




### <a name="type-ownership">ownership()</a> ###


__abstract datatype__: `ownership()`




### <a name="type-trx_fun">trx_fun()</a> ###



<pre><code>
trx_fun() = fun(() -&gt; term())
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#acquire_ownership-1">acquire_ownership/1</a></td><td></td></tr><tr><td valign="top"><a href="#acquire_ownership-2">acquire_ownership/2</a></td><td></td></tr><tr><td valign="top"><a href="#info-0">info/0</a></td><td></td></tr><tr><td valign="top"><a href="#new-0">new/0</a></td><td></td></tr><tr><td valign="top"><a href="#release_ownership-1">release_ownership/1</a></td><td></td></tr><tr><td valign="top"><a href="#transaction-2">transaction/2</a></td><td></td></tr><tr><td valign="top"><a href="#transaction-3">transaction/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="acquire_ownership-1"></a>

### acquire_ownership/1 ###


<pre><code>
acquire_ownership(Lock::<a href="#type-lock">lock()</a>) -&gt; {ok, <a href="#type-ownership">ownership()</a>} | {error, timeout}
</code></pre>

<br></br>



<a name="acquire_ownership-2"></a>

### acquire_ownership/2 ###


<pre><code>
acquire_ownership(Lock::<a href="#type-lock">lock()</a>, Timeout::timeout()) -&gt; {ok, <a href="#type-ownership">ownership()</a>} | {error, timeout}
</code></pre>

<br></br>



<a name="info-0"></a>

### info/0 ###


<pre><code>
info() -&gt; [{allocated_locks | allocated_ownerships | acquired_locks | contention, non_neg_integer()} | {has_lockfree_counters | has_lockfree_ownership, boolean()}, ...]
</code></pre>

<br></br>



<a name="new-0"></a>

### new/0 ###


<pre><code>
new() -&gt; <a href="#type-lock">lock()</a>
</code></pre>

<br></br>



<a name="release_ownership-1"></a>

### release_ownership/1 ###


<pre><code>
release_ownership(Ownership::<a href="#type-ownership">ownership()</a>) -&gt; ok | {error, not_allowed} | {error, already_released}
</code></pre>

<br></br>



<a name="transaction-2"></a>

### transaction/2 ###


<pre><code>
transaction(Lock::<a href="#type-lock">lock()</a>, Fun::<a href="#type-trx_fun">trx_fun()</a>) -&gt; {ok, FunResult::term()} | {error, timeout}
</code></pre>

<br></br>



<a name="transaction-3"></a>

### transaction/3 ###


<pre><code>
transaction(Lock::<a href="#type-lock">lock()</a>, Fun::<a href="#type-trx_fun">trx_fun()</a>, Timeout::timeout()) -&gt; {ok, FunResult::term()} | {error, timeout}
</code></pre>

<br></br>



