---
slug: 2022-07-26-the-ghcjs-linker
title: The GHCJS Linker
date: July 26, 2022
authors: [ luite ]
tags: [ ghc, javascript, linking ]
---

## Introduction

I recently gave a short presentation on the workings of the GHCJS linker. This post is a summary of the content.

## JavaScript "executables"

The task of a linker is collecting and organizing object files and resources into a loadable library or executable program. JavaScript can be run in various environments, for example the browser or node.js, and not in all of these the concept of an executable makes sense.

Therefore, when we link a Haskell program, we generate a `jsexe` directory filled with various files that allow us to run the JavaScript result:

| File        | Description          |
| :----:        | :---:             |
| `out.js`      | compiled/linked Haskell code          |
| `out.frefs.*` | list of foreing calls from `out.js` |
| `out.stats`   | source code size origin statistics for `out.js` |
| `lib.js`      | non-Haskell code, from `js-sources` in packages and RTS. possibly preprocessed |
| `rts.js`      | generated part of RTS (apply functions and similarly repetitive things) |
| `runmain.js`  | single line just starts `main` |
| `all.js`      | complete runnable program, created by combining `out.js`, `lib.js`, `rts.js` and `runmain.js` |

Most of the work done by the linker is producing `out.js`, and that's what we'll be focussing on in the next sections.

## Building `out.js`

The linker builds `out.js` by collecting all code reachable from `main` (and a few other symbols required by the RTS) and generating the required initialization code for all top-level data. The code is found in object files. These object files have the following structure:

| Section        | Description          |
| :----:        | :---:             |
| Header       | version number and offsets of other sections       |
| String table | shared string table, referred to by `Dependencies` and `Code`, to avoid duplication in file and memory |
| Dependencies | Dependency data, internally between binding groups and externally to symbols in other object files |
| Code         | Compiled Haskell code stored as serialized JavaScript AST and metadata. Code is organized in binding groups |

The object files contain binding groups of mutually dependent bindings. These are the smallest units of code that can be linked. Each binding group has some associated metadata required for initialization of the heap objects in the group. The metadata contains for example constructor tags (e.g. 1 for `Nothing`, 2 for `Just`), the arity of functions and static reference tables.

From a high level, the procedure that the linker follow is this:

| Step |
| :---: |
| Read object files from dependencies into memory |
| Decode dependency part of all object files in dependencies (includes reading the string tables) |
| Using dependency data, find all code reachable from `main` |
| Decode reachable binding groups |
| Render AST to JavaScript |
| Construct initializers from metadata | 

We avoid decoding (deserializing) the binding groups that do end up in the linked result to keep the memory consumption lower. Still the linker requires a lot of memory for larger programs, so we may need to make more improvements in the future.

## The Compactor

The compactor is an optional link-time transformation step that reduces code size. It consists of a lightweight (i.e. no expensive operations like dataflow analysis) rewrite of the code contained in the object files. The compactor is disabled when linking with the `-debug` flag. There are a few steps involved.

### Renaming private symbols

Haskell modules have a bunch of public names, i.e. those exported from the module, and private names, non-exported bindings and new bindings introduced by the optimizer or other parts of the compilation pipeline.

Private symbols are only referred to from within the same module. It doesn't matter witch JavaScript name we pick for them, as long as there are no overlaps between the names from different modules. The compactor renames all the private symbols using a global sequence to ensure short names that do not overlap.

### Block Initializer

Without the compactor, the linker generates an `h$initObj` initialization call (or `h$o`) call for each global Haskell heap value. The code for this can get quite big. The compactor collects all heap objects to be initialized in a single large array and encodes the metadata in a string. This makes the initialization code much more compact.

### Deduplication

An optional step in the compactor is deduplication of code. When deduplication is enabled with the `-dedupe` flag, the compactor looks for functionally equivalent pieces of JavaScript in the output and merges them. This can result in a significant reduction of code size.

## Incremental Linking

The linker supports building programs that are loaded incrementally. This is used for example for Template Haskell. The process that runs the Template Haskell stays alive during compilation of a whole module. When the first Template Haskell expression is compiled, it is linked against all its dependencies and the RTS and sent over to be run in the evaluator process.

Subsequent Template Haskell expressions are evaluated in the same process, where the RTS is already loaded by then, and also the dependencies of the earlier Template Haskell expressions. Therefore each expression is only linked against dependecies that are not already loaded in the evaluator process. The linker keeps track of which dependencies have already been linked.

It's also possible for users to use this functionality directly, with the `-generate-base` to create a "linker state" file along with the regular `jsexe` files. Another program can then be linked with `-use-base=state_file`, resulting in a program which leaves out everything already present in the first program.

## Future Improvements

Memory consumption is the biggest problem in the linker at the moment. Possible ways to achieve this are compression, more efficient representation of the data structures or more incremental loading of the parts from the object files that we need.

In terms of functionality, we don't take advantage of JavaScript modules yet. It would be good if we could improve the linker to support linking a library as a JavaScript module. We should also consider making use of `foreign export javascript` for this purpose.
