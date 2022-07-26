---
slug: 2022-07-18-lightweight-threads-on-JavaScript
title: Lightweight Haskell Threads on JavaScript
date: July 18, 2022
authors: [ luite ]
tags: [ghc, javascript, concurrency, ffi ]
---

## Introduction

I recently gave a short presentation on the topic of threads in GHCJS to the GHC team at IOG. This blog post is a summary of the content.

## JavaScript and Threads

JavaScript is fundamentally single threaded. There are ways to share specific data between tasks but it's not possible to run multiple threads that have access to a shared memory space of JavaScript data.

The single JavaScript thread is often responsible for multiple tasks. For example a node.js server handles multiple simultaneous connections and a web application may be dealing with user input while downloading new data in the background.

This means that any single task should take care to never block execution of the other task. JavaScript's canonical answer is to use asynchronous programming. A function reading a file returns immediately without waiting for the file data to be loaded in memory. When the data is ready, a user-supplied callback is called to continue processing the data.

## Haskell Threads

Concurrent Haskell supports lightweight threads through `forkIO`. These threads are scheduled on top of one more more operating system thread. A blocking foreign call blocks an OS thread but other lightweight threads can still run on other OS threads if available.

There is no built-in support for foreign calls with a callback in the style of JavaScript. Functions imported with `foreign import ccall interruptible` can be interrupted by sending an asynchronous exception to the corresponding lightweight thread.

## Lightweight Threads in JavaScript

GHCJS implements lightweight threads on top of the single JavaScript thread. The scheduler switches between threads and handles synchronization through `MVar` and `STM` as expected from other Haskell platforms.

Foreign calls that don't block can be handled in the usual way. We extend the foreign function interface with a new type `foreign import javascript interruptible` that conveniently supports the callback mechanism used by JavaScript frameworks. The foreign call is supplied with an additional argument `$c` representing a callback to be called with the result when ready. From the Haskell side the corresponding lightweight thread is blocked until `$c` is called. This type of foreign call can be interrupted with an asynchronous exception to the lightweight Haskell thread.

By default, Haskell threads in the JS environment run asynchronously. A call to `h$run` returns immediately and starts the thread in the background. This works for tasks that does not require immediate actions. For situations that require more immediate action, such as dealing with event handler propagation, there is `h$runSync`. This starts a synchronous thread that is not interleaved with other task. If possible, the thread runs to completion before the call to `h$runSync` returns. If the thread blocks for any reason, such as waiting for an `MVar` or a `foreign import javascript interruptible` call, synchronous execution cannot complete. The blocking task is then either interrupted with an exception or the thread is "demoted" to a regular asynchronous thread.

## Black Holes

When a Haskell value is evaluated, its heap object is overwritten by a black hole. This black hole marks the value as being evaluated and prevents other threads from doing the same. "black holing" can be done either immediately or "lazily", when the garbage collector is run. GHCJS implements immediate blackholing.

Black holes give rise to an interesting problem in the presence of synchronous and asynchronous threads. Typically if we use `h$runSync`, we want to have some guarantee that at least part of the task will run succesfully without blocking. For the most past it's fairly clear which parts of our task depends on potentially blocking IO or thread synchronization. But black holes throw a spanner in the works: Suddenly any "pure" data structure can be a source of blocking if it is under evaluation by another thread.

To regain some predictability and usability of synchronous threads, the `h$runSync` scheduler can run other Haskell threads in order to "clear" a black hole. The process ends all black holes have been cleared or when any of the black holes is impossible to clear because of a blocking situation.

This all happens transparantly to the caller of `h$runSync`, if the black holes could be cleared it appears as if they were never there.

## Conclusion

We have lightweight Haskell threads in the single-threaded JavaScript environment and extend the foreign function interface to easily support foreign calls that depend on an asynchronous callback. This way, only the Haskell lightweight thread blocks.

By default, Haskell threads are asynchronous and run in the background: The scheduler interleaves the tasks and synchronization between threads. For situations that require immediate results or actions there are synchronous threads. Synchronous threads cannot block and are not interleaved with other tasks except when a black hole is encountered.