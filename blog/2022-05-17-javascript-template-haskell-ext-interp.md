---
slug: 2022-05-17-javascript-template-haskell-external-interpreter
title: JavaScript, Template Haskell and the External Interpreter
date: May 17, 2022
authors: [ luite ]
tags: [ghc ghcjs javascript tooling profiling]
---

## Introduction

At IOG DevX we have been working on integrating various bits of GHCJS into GHC, with the goal of having a fully working JavaScript backend for the 9.6 release. For some parts this has mostly consisted of an update of the code to use the newer GHC API and dependencies. Other bits, like the Template Haskell runner, need more work.

This post gives an overview of the existing approaches for running Template Haskell in GHC based cross compilers and our plan for the JavaScript backend. Hopefully we can revisit this topic once all the work has been done, and see what exactly we ended up with.

## The Template Haskell Runner

When I first worked on Template Haskell (TH) support for GHCJS, there was no mechanism to combine Template Haskell with cross compilation in GHC.

Normally, Template Haskell is run by loading library code directly into the GHC process and using the bytecode interpreter for the current module. Template Haskell can directly access GHC data structures through the `Q` monad. Clearly this would not be possible for GHCJS: We only have JavaScript code available for the libraries and the organization of the JavaScript data structures is very different from what GHC uses internally.

So I had to look for an alternative. Running Template Haskell basically consists of two parts:

   1. loading/executing the TH code
   2. handling compiler queries from the TH code, for example looking up names or types

The queries from the TH code are abstracted by the `Quasi` typeclass. I noticed that none of the methods required passing around functions or complicated data structures, so it would be possible to serialize each request and response and send it to another process.

So I went ahead and implemented this approach with a script `thrunner.js` to load and start the code in a node.js server, a message type with serialization, and a new instance of the `Quasi` typeclass to handle the communication with the compiler via the messages. This is still what's in use by GHCJS to this day. Every time GHCJS encounters Template Haskell, it starts a `thrunner` process and the compiler communicates with it over a pipe.

After starting `thrunner.js` GHCJS sends the Haskell parts of the Template Haskell runnner to the script. This includes the runtime system and the implementation of the `Quasi` typeclass and communication protocol. After that, the TH session starts. A typical TH session looks as follows:

| Compiler | thrunner |
| :---     | :----    |
| `RunTH THExp <js code> <source location>` | |
| | `LookupName (Just <name-string>)` |
| `LookupName' (Just <name>)` |
| | `Reify <name>` |
| `Reify' <name-info>` | |
| | `RunTH' <result>` |
| `RunTH THDec <js code> <source location>` | |
| | `AddTopDecls <declarations>` |
| `AddTopDecls'` | |
| | `RunTH' <result>` |
| `FinishTH True` | |
| | `FinishTH' <memory-consumption>` |

Each message is followed up by a corresponding reply. The first `RunTH` message contains the compiled JavaScript for the Template Haskell Haskell code, along with its dependencies. Each subsequent `RunTH` only includes dependencies that have not already been sent.

The `thrunner` process stays alive during the compilation of at least an entire module, allowing for persistent state (`putQ`/`getQ`).

## External Interpreter

If we build a Haskell program with (cost centre) profiling, the layout of our data structures changes to include bookkeeping of cost centre information. This means that we need a special profiling runtime system to run this code.

What can we do if we want to run our profiled build in GHCi or Template Haskell? We cannot load compiled profiling libraries into GHC directly; its runtime system expects non-profiled code. We could use a profiled version of the compiler itself, but this would make all compilation very slow. Or we could somehow separate the profiled code of our own program from the non-profiled code in the compiler.

This was Simon Marlow's motivation for adapting the GHCJS `thrunner` approach, integrating in GHC and extending it it to support GHCi and bytecode. This functionality can be activated with the `-fexternal-interpreter` flag and has been available since GHC version 8.0.1. When the external interpreter is activated, GHC starts a separate process, `iserv` (customizable with the `-pgmi` flag) which has the role analogous to the `thrunner` script for GHCJS.

Over time, the `iserv` code has evolved with GHC and has been extended to include more operations. By now, there are quite a few differences in features:

| Feature | thrunner | iserv |
| :---        |    :----:   |          ---: |
| Template Haskell support | yes       | yes   |
| GHCi   | no | yes |
| Debugger | no | yes |
| Bytecode | no | yes |
| Object code | through pipe | from file |
| Object code linking | compiler | iserv process |

`thrunner` is not quite as complete as `iserv`: It lacks GHCi and the debugger, and there is no bytecode support. But these features are not essential for basic Template Haskell.

## Proxies and Bytecodes

Clearly it isn't ideal to have multiple "external interpreter" systems in GHC, therefore we plan to switch from `thrunner` to `iserv` for the upcoming JavaScript backend. We don't need the debugger or GHCi support yet, but we do need to adapt to other changes in the infrastructure. So what does this mean in practice?

The biggest change is that we have to rework the linker: `thrunner` does not contain any linking logic by itself: GHCJS compiles everything to JavaScript and sends compiled code to the `thrunner` process, ready to be exectuted. In contrast, `iserv` has a loader for object and archive files. When dependencies need to be loaded into the interpreter, GHC just gives it the file name. Additionally, `iserv` contains a bytecode interpreter, which `thrunner` lacks.

Another change is using the updated message types. In the `thrunner` session example above we could see that each message is paired with a response. For example a `RunTH'` response always follows a `RunTH` message, with possibly other messages in between. `iserv` has an interesting approach for the `Message` datatype: Instead of having pairs of data constructors for each message and its response, `iserv` has a GADT `Message a`, where the `a` type parameter indicates the expected response payload for each data constructor.

During development of the `thrunner` program it turned out to be very useful to save and replay Template Haskell sessions for debugging purposes. We'd like to do this again, but now saving the message in a readable/writable format. Since we're dealing with JavaScript, JSON appears to be the obvious choice.

Our plan is to have an `iserv` implementation that consists of a JavaScript part that runs in node.js and a proxy process to handle communication with GHC. The proxy process converts the messages between GHC's own (`binary` based) serialization format and JSON. The proxy process is relatively simple, but it does reveal one downside of the new GADT based message types: A proxy is stateful. We must always know which message we have sent to convert the response back from JSON to `binary`.

It's not yet known whether we will implement a full bytecode interpreter. We expect it to become clear during implementation whether we can get away without one early on.

## Conclusion

We have seen how Template Haskell and GHCi code can be run outside the GHC process for profiling or cross compiling, with both the `thrunner` approach in GHCJS and the newer `iserv` in GHC.

We at IOG DevX are working on switching to the `iserv` infrastructure for the upcoming GHC JavaScript backend, which involves a substantial rewrite, mainly because of differences in linking. This is a work in progress, and we intend to revisit this topic in another blog post once the final design has been implemented.
