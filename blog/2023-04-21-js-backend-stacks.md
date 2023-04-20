---
slug: 2023-04-21-stacks-in-the-js-backend
title: Stacks in the JavaScript Backend
date: April 21, 2023
authors: [ luite ]
tags: [ghc, javascript, threads, rts ]
---

## Introduction

I recently gave a short presentation on the topic of stacks in the GHC JavaScript backend to the GHC team at IOG. This blog post is a summary of the content.

In the context of a program produced by the GHC JavaScript backend, two different types of stack exist: The JavaScript call stack and Haskell lightweight stacks. In this post we will focus mostly on the lightweight stacks.

First we will see why using only the JavaScript call stack is not suitable for running compiled Haskell code. Then we will introduce the calling convention we use for Haskell and see how the lightweight stacks are used for making calls and passing around data. After this, we will explore in more detail how they are used for exception handling and multithreading.

## Lightweight Stacks

GHC's JavaScript backend translates Haskell, via the intermediate language STG, to JavaScript functions and data. When compiled Haskell code needs to run a computation from a different part of the code, it has to call into the corresponding (JavaScript) function. The code relies on tail calls a lot, and for this reason we cannot use a regular JavaScript function call: JavaScript does not support tail-call optimization and we would run out of stack space quickly.

Instead, we use a technique known as "trampolining" to avoid using space on the JavaScript call stack.

### Trampolining

When using trampolining to make a tail call, we don't directly make a function call (which would use JavaScript stack space), instead we return the function itself, and let a "scheduler" do the call for us. This is easiest to show with an example:

```javascript
// example without trampolining
function example1_direct() {
  ... // compute arg1 here

  // make a tail call to xyz (using space on the JavaScript call stack)
  return xyz(arg1);
}
```

`example1_direct` above uses a direct call to `xyz`. If we change it to use trampolining to call `xyz` it looks as follows:

```javascript
// simplified scheduler function
function scheduler(c) {
  // scheduler trampolining loop:
  while(true) {
    c = c();
  }
}

// example1 is called from the scheduler loop
function example1_trampoline() {
  ... // compute arg1 here

  // use global "register" variable h$r1 for the argument
  h$r1 = arg1;

  // return a reference to the function to make the call
  return xyz;
}
```

`example1_trampoline` only works if it's called from the scheduler loop. The scheduler loop keeps calling functions (`c = c();`), so all calls are made directly from the `scheduler` function, no growing JavaScript stack.

The scheduler loop cannot deal with function arguments, it calls each `c` without any arguments. There's no way to efficiently return both a function and arguments to `scheduler` in JavaScript. Allocating an object wrapping the function and arguments for each call would be prohibitively expensive. Therefore we rely on global variables `h$r1, h$r2, h$r3, ...`, which we refer to as "registers", to pass the arguments to `xyz`.

### Continuations

Other situations are a bit more complicated, for example if our code needs to do something with the result of the call, like pattern matching on a data constructor or primitive value. Let's extend our example a little bit:

```javascript
function abc_direct(arg1) {
   var result = ... // compute result
   return result;
}

function example2_direct() {
  ... // compute arg1
  var r = abc_direct(arg1);
  if(r > 0) {
    return 1;
  } else {
    return 0;
  }
}
```

Here the call to `abc_direct` is not a tail call; `example2_direct` has to do something with the result of the call. To make a trampolining version out of `example2`, we need to consider the following:

- `abc` has to return a value to the function that called it
- `example2` needs to inspect the value returned by `xyz`.

Returning the result value directly from `abc` is not possible: It would end up in the `c = c();` loop in `scheduler`. We need to return a value, but we can only return a function to make a tail call. Where do we get this function?

This is where the lightweight Haskell stack comes into play. We use the convention that when we are done computing, we "return" the result by making a tail call to a _continuation_ (function) at the top of the lightweight stack, passing the result as an argument.

The stack is stored in the global variable `h$stack`, which is a JavaScript array, and we have a global integer variable `h$sp` which represents the index of the top of the stack. This means that our continuation is found at `h$stack[h$sp]`, the top of the stack. We call it the usual way: Returning the function to the trampoline and using registers (`h$r1, h$r2, ...`) for the arguments.

Using this convention, `example2` becomes:

```javascript
function abc_trampoline() {
  var arg1 = h$r1;
  var result = ... // compute result

  // call the continuation on the stack with the result
  h$r1 = result;
  return h$stack[h$sp];
}

function example2_trampoline() {
  ... // compute arg1

  // push our continuation
  h$sp++;
  h$stack[h$sp] = example2_cont;

  // tail-call abc through trampoline
  h$r1 = arg1;
  return abc_trampoline;
}

function example2_cont() {
  // pop this continuation from the stack
  h$sp--;

  // the result is the first register argument
  var r = h$r1;

  if(r > 0) {
    h$r1 = 1;
  } else {
    h$r1 = 0;
  }

  // make the tail call with the result in h$r1
  return h$stack[h$sp];
}
```

Having a stack of continuations allows calls of arbitrary depth to be composed.

### Stack Frames

Sometimes just storing continuations isn't enough. Suppose that we need to use some data in our continuation, for example:

```javascript
function example3_direct() {
  var arg1 = ... // compute arg1
  var a    = ... // compute a
  var b    = ... // compute b
  var r = abc_direct(arg1);
  if(r > 0) {
    return a;
  } else {
    return b;
  }
}
```

Here we need to make sure that `a` and `b` are available in the continuation. To do so, we save them on the stack as follows:

```javascript
function example3_trampoline() {
  arg1 = ... // compute arg1
  a    = ... // compute a
  b    = ... // compute b

  // push our continuation, saving a and b
  h$sp += 3;
  h$stack[h$sp-2] = a;
  h$stack[h$sp-1] = b;
  h$stack[h$sp] = example3_cont;

  // tail-call abc through trampoline
  h$r1 = arg1;
  return abc_trampoline;
}

function example3_cont {
  var r = h$r1;

  // restore a and b from stack
  var a = h$stack[h$sp-2];
  var b = h$stack[h$sp-1];

  // pop the stack frame (3 slots: a, b, example3_cont)
  h$sp -= 3;

  // return our result through the trampoline
  if(r > 0) {
    h$r1 = a;
  } else {
    h$r1 = b;
  }
  return h$stack[h$sp];
}
```

Now the stack consists of JavaScript functions (continuations), each followed by zero or more slots of data. We refer to the JavaScript function as the _header_ and the data as the _payload_. The header and payload combined are called a _stack frame_.

Here is an example of what `h$stack` might look like during the execution of `example3_trampoline`:

| Slot | Value                                  | Frame | Type / Description |
| ---- | -------------------------------------- | ----- | ----------------- |
| 0    | `h$done`                               | __1__ | `function`, "finished" frame |
| 1    | `0`                                    | 2     | `number`, flags |
| 2    | `h$...zireportError`                   | 2     | `object`, exception handler |
| 3    | `h$catch_e`                            | __2__ | `function`, exception catch frame |
| 4    | `4`                                    | 3     | value of `a` (in `example3` above) |
| 5    | `5`                                    | 3     | value of `b` (in `example3` above) |
| 6    | `example3_cont`                        | __3__ | `function`, `example3_cont` frame |

Frame `3` is the `example3_cont` frame with two slots of payload, pushed by our example. Frames `2` and `1` are added by the runtime system. We will discuss them in more detail later.

### Stack Frame Metadata

During normal execution of a program, the code that manipulates the stack has knowledge of the specific stack frame it's working with: It knows the size of the payload and which values are stored in each stack slot. However, there are also operations that require dealing with all kinds of unknown stack frames. Exceptions and software transactional memory are the most important ones.

These operations deal with unknown stack frames and sometimes need information about the frame, for example the frame size. Where do we store it?

Every JavaScript function is also an object. This means that we can store arbitrary data in the function's properties. For example for any function `f` we can do `f.x = 5;`. This is what we use to store the metadata for stack frames.

Stack frames contain at least the following metadata in the header:

| Property   | Description |
| ----------- | ----------- |
| `f.size`      | _integer_, number of payload slots in stack frame       |
| `f.a`         | _integer_, number of register variables used for arguments        |

`example3_cont.size` would be `2`, since it has two slots of payload. The size of the full `example3_cont` frame is three slots.

There is also a small number of frames (defined by the runtime system) with the value `f.size < 0`. These have the stack frame size stored inside the payload. `h$ap_gen` is another special case, where the stack frame payload encodes both frame size and register information. The runtime system function `h$stackFrameSize` computes the size of a stack frame:

```javascript
// compute the size of stack frame h$stack[h$sp] with header f
function h$stackFrameSize(f) {
  if(f === h$ap_gen) {
    return (h$stack[h$sp - 1] >> 8) + 2;
  } else {
    var tag = f.size;
    if(tag < 0) {
      return h$stack[h$sp-1];
    } else {
      return (tag & 0xff) + 1;
    }
  }
  ```

Here is an example of a stack with an `h$ap_gen` stack frame, the payload is stored in slots `4` and `5`, while slot `6` contains the metadata:

| Slot | Value                                  | Frame | Type / Description |
| ---- | -------------------------------------- | ----- | --------------- |
| 0    | `h$done`                               | __1__ | `function`, "finished" frame |
| 1    | `0`                                    | 2     | `number`, flags |
| 2    | `h$...zireportError` | 2     | `object`, exception handler |
| 3    | `h$catch_e`                            | __2__ | `function`, exception catch frame |
| 4    | `4`                                    | 3     | payload |
| 5    | `3`                                    | 3     | payload |
| 6    | `513`                                  | 3     | `number`, stack frame metadata (payload size and registers) |
| 7    | `h$ap_gen`                             | __3__ | `function`, `h$ap_gen` frame |

## Exception Handling

Haskell allows exceptions to be thrown within threads and between threads as an alternate way of returning a value. The _throw_ operation transfers control to exception handler in the next `catch` frame on the stack.

The `catch` frame has two words of payload:

| Slot | Value                                  | Frame | Type / Description |
| ---- | -------------------------------------- | ----- | ----------------   |
| 0    | `0`                                    | 1     | `number`, flags |
| 1    | `h$...zireportError` | 1     | `object`, exception handler |
| 2    | `h$catch_e`                            | __1__ | `function`, exception catch frame |


The code for `h$catch_e` is straightforward, it just pops the stack frame and returns to the next frame. This is what happens if no exception has occurred; the program just skips past the exception handler:

```javascript
function h$catch_e() {
  h$sp -= 3;
  return h$stack[h$sp];
};
```

An exception is thrown by the `h$throw` function, which unwinds the stack. Its implementation in simplified form looks like this:

```javascript
// throw exception e
function h$throw(e) {
  ...
  while(h$sp > 0) {
    f = h$stack[h$sp];
    ...
    // check for stack frames that need to be handled
    if(f === h$catch_e) break;
    if(f === h$atomically_e) { ... }
    if(f === h$catchStm_e) break;
    if(f === h$upd_frame) { /* handle black hole */ }
    h$sp -= h$stackFrameSize(f);
  }
  if(h$sp > 0) {
    var maskStatus = h$stack[h$p - 2];
    var handler = h$stack[h$sp - 1];
    // jump to handler
    ...
  } else {
    // no exception handler found, report error
    ...
  }
}
```

`h$throw` keeps removing stack frames from the stack until some frame of interest is found, using `h$stackFrameSize` to determine the size of each frame. Eventually, it transfers control to an exception handler or it reports an error if no exception handling frame could be found.

## Threads

Concurrent Haskell supports multiple Haskell threads. These Haskell threads are run by one or more system threads. The JavaScript "system" is single-threaded (ignoring Web Workers, which have limited memory sharing), so we have to use a single system thread to run everything. It turns out to be quite straightforward to support multithreading if we use the trampolining calling convention with lightweight stacks. Each Haskell thread gets its own stack and stack pointer.

Each Haskell thread has a thread state object, `t` of type `h$Thread`. This object contains the stack (`t.stack`, `Array`) and stack pointer (`t.sp`, `number`) for the thread, and also keeps track of the thread status, for example whether the thread is finished or masking asynchronous exceptions.

When a thread is created, the `h$Thread` object is initialized as follows:

```javascript
/** @constructor */
function h$Thread() {
    // get a unique thread id
    this.tid = ++h$threadIdN;
    this.status = THREAD_RUNNING;

    // some initial error handling frame on the stack
    this.stack = [h$done
                 , 0
                 , h$baseZCGHCziConcziSynczireportError
                 , h$catch_e
                 ];
    this.sp = 3;

    this.excep = [];              // waiting async exceptions
    this.mask = 0;                // async exceptions masked
                                  // (0 unmasked, 1: uninterruptible, 2: interruptible)
    this.interruptible = false;   // currently in an interruptible operation

    ...
}
```

The initial stack contains two stack frames. The top three slots contain a `catch` frame with the `h$catch_e` header, the `h$baseZCGHCziConcziSynczireportError` exception handler and `0`, for the mask state. The last slot of the stack is for `h$done` frame, which only has a header and no payload.

### Scheduling and Suspending a Thread

Now we have potentially multiple `h$Thread` objects, each with their own stack. But how do we run the threads?

The trampolining calling convention introduced earlier expects the stack to be stored in `h$stack`, with `h$sp` the index of the topmost used slot. This means that if we want to run a thread `t`, we need to set `h$stack` and `h$sp` to the values from the `h$Thread` object `t`:

```javascript
// scheduling a thread t
function __scheduleThread(t) {
   h$currentThread = t;
   h$stack = t.stack;
   h$sp = t.sp;

   ...
}
```

We also store `t` itself in the global `h$currentThread` variable to keep track of the currently running thread.

While the thread is running, `h$stack` and `h$sp` are constantly updated by the compiled Haskell code, so they go out of sync with values saved in the `h$Thread` object. This means that when we suspend a thread, these need to be copied back into the `h$Thread` as follows:

```javascript
// suspending the current thread
function __suspendCurrentThread() {
    // save the global h$stack and h$sp back into the thread state object
    h$currentThread.stack = h$stack;
    h$currentThread.sp = h$sp;

    // set h$currentThread to indicate that no thread is running.
    h$currentThread = null;

    ...
}
```

Having multiple threads, the trampoline loop looks a bit different. A thread returns a special continuation `h$reschedule` to indicate that another thread may now be scheduled. In simplified form, the scheduler looks as follows:

```javascript
function scheduler() {
  while(true) {
     // fetch the next thread to run and schedule it
     var t = getNextThread();

     // if there are no more threads then we're done
     if(t === null) return;

     __scheduleThread(t);

     // start by executing the continuation at the top of the stack
     var c = h$stack[h$sp];

     // run until the thread indicates that another may now be scheduled
     while(c !== h$reschedule) {
         c = c();
     }
     // suspend the thread again
     __suspendCurrentThread();
  }
}
```

In practice, the scheduler is quite a bit more complicated. For example it also uses time-based switching, changing to different thread even when `h$reschedule` is not returned by the current thread. In that case, the scheduler takes care of saving the thread state onto the stack, using metadata from the continuation.

A discussion of all the ins and outs of the scheduler is beyond the scope of this blog post. But it could be the topic of a follow-up post.

### Threads and Asynchronous Exceptions

Haskell exceptions come in two flavours: _synchronous_ exceptions and _asynchronous_ exceptions. Synchronous exceptions always come from the code itself, for example a pattern match failure, a call to `error` or `undefined`. Running the code again with the same input always produces the same result.

Asynchronous exceptions come from the outside. They can come from other threads (but don't need to) or from the runtime system. Typical reasons for asynchronous exceptions are timeouts and resource exhaustion. It's perfectly possible for the same function with the same input to be aborted with an asynchronous exception the first run, while running to completion the second time. This means that we must be careful preserving any partially completed computation.

Asynchronous exceptions can happen at any time, which can make them quite tricky to deal with. They could leave the program in an inconsistent state if they occur at the wrong time. Therefore, threads can temporarily block asynchronous exceptions, a process called _masking_. Different masking states are used for indicating whether exceptions are still masked when the thread is performing an _interruptible_ operation.

Here is a comparison of the main differences between synchronous and asynchronous exceptions:

|             | Synchronous      | Asynchronous |
| ----------- | ----------------- | ------------ |
| __Thrown using primop__ | `raise#`, `raiseIO#` | `killThread#`   |
| __Computation on the stack__ | thunks updated to immediately raise the same exception again | stack captured in heap objects so the computation can be resumed |
| __Throw to other thread__ | impossible | push `h$raiseAsync_frame` frame onto receiving thread's stack (unless masked) |
| __Throw to current thread__ | use `h$throw` immediately | use `h$throw` immediately (no masking) |
| __Masking__ | no masking, thrown immediately | exception saved in `h$Thread.excep` of receiving thread if masked, the sending thread is blocked until the exception is delivered |

We can see that while on the surface, synchronous and asynchronous exceptions look similar, there are many differences under the hood. Masking requires some additional machinery for thread synchronization and storing exceptions that cannot be delivered yet.

| Property | Description |
| --- | ---- |
| `h$Thread.mask` | `number`, mask status indicating unmasked / masked uninterruptible / masked interruptible |
| `h$Thread.excep` | `Array`, list of unposted asynchronous exceptions and their posting `h$Thread` objects. When the receiving thread unmasks, the scheduler posts the exceptions to its `h$Thread.stack` with `h$raiseAsync_frame` and unblocks the sending threads |
| `h$Thread.interruptible` | `boolean`, interruptible state of the thread |

## Important Functions and Values

We have seen various global names and code examples so far, but some example code was simplified a little for clarity. This section offers some pointers to get started with exploring the actual code.

#### Global Variables

| Name      | Description |
| ----------- | ----------- |
| `h$stack`   | `Array`, the stack of the currently running thread      |
| `h$sp`      | `integer`, index of the top slot of the stack   |
| `h$r1`, `h$r2`, ..., `h$r32` | `Any`, "registers", function arguments (first 32) |
| `h$regs` | `Array`, function arguments when more than 32 are needed |
| `h$currentThread` | `h$Thread`, object of currently running Haskell thread. `null` if no Haskell thread is running |

#### Running Threads

These functions start a new thread given an `IO` action. They also start the main loop of the scheduler.

| Name | Description |
| ----------- | ----------- |
| `h$run(c)` | Run `IO` action `c` in a new lightweight thread |
| `h$main(c)` | Run `IO` action `c` in a new lightweight thread, flush buffers and exit the program when finished |

#### Some Useful Internal Functions

| Name | Description |
| ----------- | ----------- |
| `h$stackFrameSize(f)` | Compute the size of the stack frame `f` |
| `h$throw(e, async)`   | Throw exception `e` (`object`), `async` (`boolean`) determines whether the exception is asynchronous. |
| `h$mainloop` | The entry point for running Haskell code |
| `h$runThreadSlice` | The actual trampolining loop (for asynchronous threads) |
| `h$scheduler` | Picks the next thread to schedule, schedules it and returns the continuation to call |
| `h$suspendCurrentThread` | Saves the thread state registers onto the stack to allow another thread to be run, used in conjunction with `h$restoreThread` |
| `h$reschedule` | Tells the scheduler to schedule another thread |

#### Some Common Stack Frames

| Name | Description |
| ----------- | ----------- |
| `h$done` | Finish the current thread |
| `h$return` | Load a value from the stack into `h$r1` |
| `h$restoreThread` | Load values from stack frame into registers, used to restore the thread state |
| `h$catch_e` | Catch an exception |
| `h$ap_n_m_e` | Apply a function to `m` arguments taking `n` slots on the stack |
| `h$raiseAsync_frame` | Raise an asynchronous exception received by this thread |
| `h$ap_gen` | Generic apply a function, the number of arguments is in the stack frame |


## Conclusion

We have introduced the _trampolining_ calling convention used by the JavaScript backend for GHC and the structure of the stacks used by it.

We have seen that stacks of Haskell lightweight threads are represented by JavaScript arrays with the JavaScript backend. The contents on the stack consist of stack frames with a header and a payload. The header of each stack frame contains some metadata so that code for exception handling can traverse the stack and transfer control to an exception handler.
