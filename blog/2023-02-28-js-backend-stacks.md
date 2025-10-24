---
slug: 2023-02-28-stacks-in-the-js-backend
title: Stacks in the JavaScript Backend
date: Febuary 28, 2023
authors: [ luite ]
tags: [ghc, javascript, threads, rts ]
---

## Introduction

I recently gave a short presentation on the topic of stacks in the JavaScript backend to the GHC team at IOG. This blog post is a summary of the content.

## Haskell Lightweight Stacks

In the context of a program produced by the GHC JavaScript backend, two different types of stack exist: The JavaScript call stack and Haskell lightweigt thread stacks. This blog post deals with the latter.

Each Haskell thread has a thread state object, `t` of type `h$Thread`. This object stores the state of a lightweight thread, for example whether the thread is finished or whether asynchronous exceptions are ignored. It also contains `t.stack`, an array representing the stack and `t.sp`, a number pointing to the current top of the stack.

`t.stack` grows dynamically as needed, and is occasionally shrunk to reclaim memory.

When a thread is created, the stack is initialized with some values:

```javascript
/** @constructor */
function h$Thread() {
    this.tid = ++h$threadIdN;
    this.status = THREAD_RUNNING;
    this.stack = [h$done
                 , 0
                 , h$baseZCGHCziConcziSynczireportError
                 , h$catch_e
                 ];
    this.sp = 3;
    this.mask = 0;                // async exceptions masked (0 unmasked, 1: uninterruptible, 2: interruptible)
    this.interruptible = false;   // currently in an interruptible operation
    ...
}
```

The initial stack contains two stack frames. The top three slots contain a `catch` frame with the `h$catch_e` header, the `h$baseZCGHCziConcziSynczireportError` exception handler and `0`, for the mask state. The last slot of the stack is for `h$done` frame, which only has a header and no payload.

## Scheduling a Thread

Typical Haskell code does a lot of manipulation of values on the stack. It would be quite inefficient to do all of this through the thread state object of the current thread, `h$currentThread`. That's why the stack `h$stack` and the "stack pointer" `h$sp` to the top of the stack are global variables that are initialized when a thread is scheduled:

```javascript
// scheduling a thread t
h$currentThread = t;
h$stack = t.stack;
h$sp = t.sp;
```

When a thread is suspended, the values are saved back to the thread state object:

```javascript
// suspending a thread t
t.stack = h$stack;
t.sp = h$sp;
h$currentThread = null;
```

## Stack Frames

Each stack frame starts with a header, which is a JavaScript function. The header is followed by zero or more slots of payload, which can be arbitrary JavaScript values.

The header serves as the "return point": When some code is done reducing some value to weak-head normal form it returns this value to the next stack frame by storing it in `h$r1` (or more for large values or unboxed tuples), popping its own stack frame and calling the header of the next stack frame at `h$stack[h$sp]`

An example is shown below.

```javascript
function h$stackFrame_e() {
  ...
  h$r1 = somethingWHNF;
  h$sp -= 3; // pop current frame
  return h$stack[h$sp]; // return to next frame
}
```

The header also contains metadata, stored in properties of the function object. Of particular interest is the `size` property, which contains the size of the stack frame in slots. Certain operations, like throwing exceptions or restarting STM transactions need to know the size of each stack frame to be able to "unwind" the stack.

Almost all stack frames have their size stored in the `size` property of the header. An exception is the `h$ap_gen` frame, which contains an arbitrary size function application. This frame type does not have a fixed size, and the size is stored in the payload of the frame itself. Frames `f` with the size stored in they payload of the frame have `f.size < 0`.

## Exception Handling

During normal execution of a program, the code that manipulates the stack has knowledge of the specific stack frame it's working with: It knows which values are stored in each stack slot. However there are also operations that require dealing with all kinds of unknown stack frames. Exceptions and STM are the most important ones.

Haskell allows exceptions to be thrown within threads and between threads as an alternate way of returning a value. The throw operation transfers control to exception handler in the next `catch` frame on the stack.

The `catch` frame has two words of payload:

```javascript
0 // mask status
h$baseZCGHCziConcziSynczireportError // handler
h$catch_e // header
```

The code for the header is straightforward, it just pops the stack frame and returns to the next frame. This is what happens if no exception has occurred; the code just skips past the exception handler:

```javascript
function h$catch_e() {
  h$sp -= 3;
  return h$stack[h$sp];
};
```

An exception is thrown by the `h$throw` function, which unwinds the stack. Its implementation in simplified form looks like this:

```javascript
function h$throw(e, async) {
  ...
  while(h$sp > 0) {
    f = h$stack[h$sp];
    ...
    if(f === h$catch_e) break;
    if(f === h$atomically_e) { ... }
    if(f === h$catchStm_e && !async) break;
    if(f === h$upd_frame) { /* handle black hole */ }
    h$sp -= h$stackFrameSize(f, sp);
  }
  if(h$sp > 0) {
    var maskStatus = h$stack[h$p - 2];
    var handler = h$stack[h$sp - 1];
    ...
  }
  /* jump to handler */
}
```

`h$throw` keeps removing stack frames from the stack until some frame of interest is found. Eventually it transfers control to an exception handler or it reports an error if no exception handling frame could be found. `h$throw` uses the `h$stackFrameSize` helper function do determine the size of each frame.

```javascript
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

## Conclusion

We have that stacks in the JavaScript backend are represented by JavaScript arrays. The contents on the stack consists of stack frames with a header and a payload. The header of each stack frame contains some metadata so that code for exception can traverse the stack and transfer control to an exception handler.