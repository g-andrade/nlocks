

# Module nvars #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)



<a name="types"></a>

## Data Types ##




### <a name="type-nvar">nvar()</a> ###



<pre><code>
nvar() = #nvar{}
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#dirty_exchange-2">dirty_exchange/2</a></td><td></td></tr><tr><td valign="top"><a href="#dirty_execute-2">dirty_execute/2</a></td><td></td></tr><tr><td valign="top"><a href="#dirty_load-1">dirty_load/1</a></td><td></td></tr><tr><td valign="top"><a href="#dirty_store-2">dirty_store/2</a></td><td></td></tr><tr><td valign="top"><a href="#exchange-2">exchange/2</a></td><td></td></tr><tr><td valign="top"><a href="#exchange-3">exchange/3</a></td><td></td></tr><tr><td valign="top"><a href="#execute-2">execute/2</a></td><td></td></tr><tr><td valign="top"><a href="#execute-3">execute/3</a></td><td></td></tr><tr><td valign="top"><a href="#get-0">get/0</a></td><td></td></tr><tr><td valign="top"><a href="#load-1">load/1</a></td><td></td></tr><tr><td valign="top"><a href="#load-2">load/2</a></td><td></td></tr><tr><td valign="top"><a href="#new-0">new/0</a></td><td></td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td></td></tr><tr><td valign="top"><a href="#put-1">put/1</a></td><td></td></tr><tr><td valign="top"><a href="#replace-1">replace/1</a></td><td></td></tr><tr><td valign="top"><a href="#store-2">store/2</a></td><td></td></tr><tr><td valign="top"><a href="#store-3">store/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="dirty_exchange-2"></a>

### dirty_exchange/2 ###


<pre><code>
dirty_exchange(NVar::<a href="#type-nvar">nvar()</a>, Value::term()) -&gt; PrevValue::term()
</code></pre>

<br></br>



<a name="dirty_execute-2"></a>

### dirty_execute/2 ###


<pre><code>
dirty_execute(NVar::<a href="#type-nvar">nvar()</a>, Function::fun(() -&gt; (Return::term()))) -&gt; Return::term()
</code></pre>

<br></br>



<a name="dirty_load-1"></a>

### dirty_load/1 ###


<pre><code>
dirty_load(NVar::<a href="#type-nvar">nvar()</a>) -&gt; term()
</code></pre>

<br></br>



<a name="dirty_store-2"></a>

### dirty_store/2 ###


<pre><code>
dirty_store(NVar::<a href="#type-nvar">nvar()</a>, Value::term()) -&gt; ok
</code></pre>

<br></br>



<a name="exchange-2"></a>

### exchange/2 ###


<pre><code>
exchange(NVar::<a href="#type-nvar">nvar()</a>, Value::term()) -&gt; {ok, PrevValue::term()} | {error, timeout}
</code></pre>

<br></br>



<a name="exchange-3"></a>

### exchange/3 ###


<pre><code>
exchange(NVar::<a href="#type-nvar">nvar()</a>, Value::term(), Timeout::timeout()) -&gt; {ok, PrevValue::term()} | {error, timeout}
</code></pre>

<br></br>



<a name="execute-2"></a>

### execute/2 ###


<pre><code>
execute(NVar::<a href="#type-nvar">nvar()</a>, Function::fun(() -&gt; (Return::term()))) -&gt; {ok, Return::term()} | {error, timeout}
</code></pre>

<br></br>



<a name="execute-3"></a>

### execute/3 ###


<pre><code>
execute(NVar::<a href="#type-nvar">nvar()</a>, Function::fun(() -&gt; (Return::term())), Timeout::timeout()) -&gt; {ok, Return::term()} | {error, timeout}
</code></pre>

<br></br>



<a name="get-0"></a>

### get/0 ###


<pre><code>
get() -&gt; Value::term()
</code></pre>

<br></br>



<a name="load-1"></a>

### load/1 ###


<pre><code>
load(NVar::<a href="#type-nvar">nvar()</a>) -&gt; {ok, term()} | {error, timeout}
</code></pre>

<br></br>



<a name="load-2"></a>

### load/2 ###


<pre><code>
load(NVar::<a href="#type-nvar">nvar()</a>, Timeout::timeout()) -&gt; {ok, term()} | {error, timeout}
</code></pre>

<br></br>



<a name="new-0"></a>

### new/0 ###


<pre><code>
new() -&gt; <a href="#type-nvar">nvar()</a>
</code></pre>

<br></br>



<a name="new-1"></a>

### new/1 ###


<pre><code>
new(InitialValue::term()) -&gt; <a href="#type-nvar">nvar()</a>
</code></pre>

<br></br>



<a name="put-1"></a>

### put/1 ###


<pre><code>
put(Value::term()) -&gt; ok
</code></pre>

<br></br>



<a name="replace-1"></a>

### replace/1 ###


<pre><code>
replace(Value::term()) -&gt; PrevValue::term()
</code></pre>

<br></br>



<a name="store-2"></a>

### store/2 ###


<pre><code>
store(NVar::<a href="#type-nvar">nvar()</a>, Value::term()) -&gt; ok | {error, timeout}
</code></pre>

<br></br>



<a name="store-3"></a>

### store/3 ###


<pre><code>
store(NVar::<a href="#type-nvar">nvar()</a>, Value::term(), Timeout::timeout()) -&gt; ok | {error, timeout}
</code></pre>

<br></br>



