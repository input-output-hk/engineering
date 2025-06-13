---
slug: 2022-12-07-template-haskell-for-the-js-backend
title: Template Haskell for the GHC JavaScript Backend
date: December 7, 2022
authors: [ luite ]
tags: [ghc, javascript ]
---

## Introduction

Recently, the JavaScript backend was merged into GHC. The new backend is based on the code generator from GHCJS, but comes with a large number of changes and improvements as the result of many months of work. The result is much better performance and test coverage compared to the original GHCJS codebase, however some features didn't make it into the first version. One of them is support for Template Haskell.

## Missing Features

Our focus for the initial release of the JavaScript backend has been on adding reviewable and well tested code to GHC. This meant that we made a choice to minimize changes to existing GHC infrastructure and leave out a few features that will eventually make it back at a later stage.

The most important missing features are:

- Optimizer: Local (per function) optimization. Affects code size and readability of generated JavaScript.
- Compactor: Link time optimization/deduplication and generation of compact initialization code. Affects code size and startup time.
- Foreign Function Interface placeholder syntax: Allows convenient inline JavaScript code in foreign imports, for example `$r = Math.sin($1)`.
- Incremental linker: Allows a program to be split into multiple parts, that can be loaded separately.
- Template Haskell: Template metaprogramming.

We are planning to bring back the missing features in one form or another in the coming releases. The features can be grouped into a few mostly independent tasks:

- Optimizer/Compactor: Depend on an improved Intermediate Representation; It's difficult to do the required analysis and rewrites correctly with the current JMacro based IR.
- FFI syntax: Requires a decision of the exact subset of JavaScript to support and an implementation of a parser for this.
- Incremental Linker/Template Haskell: Template Haskell depends on the incremental linker to work. Everything must fit in the "external interpreter" `iserv` framework that GHC provides.

## Template Haskell

The Template Haskell implementation in GHCJS is quite old. It was the inspiration for the "external interpreter" framework in GHC (which is used for GHCi and Template Haskell), but was never brought up to date with it. However, it still shares quite a bit of code with it.

The GHC external interpreter (`iserv`) runs in a separate process and runs Template Haskell code by executing commands received as messages from the compiler:

  1. start `iserv` program
  2. **message:** load library `base.a`
  3. **message:** load object file `module.o`
  4. **message:** load bytecode `<xyz>` and return a reference
  5. **message:** run bytecode reference and return the result

Here we use the notation `<xyz>` to indicate bytecode that is contained inside the message. The `iserv` process contains functionality to load libraries and object files directly, and it contains the GHC bytecode interpreter.

Now let's have a look at an analogous GHCJS Template Haskell session:

  1. start `thrunner.js` script with node.js
  2. **message:** load and run JavaScript code `<Template Haskell server>`
  3. **message:** load and run JavaScript code `<xyz>` and return the result

The incremental linker is used to construct the code in the messages: Initially the `<Template Haskell server>` is loaded, which includes the RTS. Every subsequent message contains only code that wasn't already loaded. For example the message to run `<xyz>` might include code from the `base` package or other dependencies, but only if that code wasn't already required by `<Template Haskell server>`.

The `thrunner.js` script does not contain a bytecode interpreter and has no functionality for loading object files. It's quite involved to load object files for the JS backend, since they need a rendering pass by the linker (and optionally compactor). Adding the bytecode interpreter is also not trivial, since it's written in C.

So how do we go about integrating everything in the `iserv` framework without implementing the complicated object loading and bytecode interpreter functionality? Work is on the way to extend the `iserv` message protocol to support the incremental linking scheme as used by GHCJS, where the compiler does all the linking work:

  1. start `iserv.jsexe` with node.js
  2. **message:** load JavaScript code `<xyz>` and return a reference
  3. **message:** run JavaScript code reference and return the result

Here the code `<xyz>` is linked incrementally against `iserv.jsexe`.[^1]

[^1]: In theory it's possible to implement most Template Haskell functionality (everything except state: `putQ`/`getQ`/`addModuleFinalizer`) without incremental linking, but performance would likely be unacceptable and it would be a big change from how `iserv` typically works.

This requires some changes in the `iserv` code, and was therefore out of scope of the original JavaScript backend merge request, where we tried to minimize the changes to existing GHC infrastructure in order to make the (already large) patch more reviewable. We expect the update to be ready before the 9.8 release, and are planning to backport the changes to 9.6.

## Conclusion

We plan to bring back the incremental linking functionality from GHCJS for use with Template Haskell to the JavaScript backend for GHC and adapt the external interpreter to accept incrementally linked JavaScript code. Most likely, the feature will be merged into GHC in time for the 9.8 release, and backported to a minor 9.6 revision afterwards.
