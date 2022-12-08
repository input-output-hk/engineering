---
slug: 2022-12-05-ghc-js-backend-merged
title: JavaScript backend merged into GHC
authors: [sylvain, doyougnu, luite, josh, moritz]
tags: [ghc, javascript, cross-compilation]
---

A new JavaScript backend has been
[merged](https://gitlab.haskell.org/ghc/ghc/-/commit/cc25d52e0f65d54c052908c7d91d5946342ab88a)
into GHC on November 30th, 2022!

In this post, we, the GHC DevX team at IOG, describe the challenges we faced
bringing GHCJS to GHC, how we overcame those challenges, and what's left to do.

[Skip to build instructions for the impatient](#build)

## Why JavaScript?

We chose to target JavaScript because JavaScript is simply the most portable
language on the web, and especially so for user interfaces. In addition to
portability, there is a large amount of mature tooling around the JavaScript
language and such a large user base that the language will remain well-supported for a long time.

WebAssembly is a promising target as well, and [Tweag](https://www.tweag.io/)
has just merged a
[WebAssembly backend](https://www.tweag.io/blog/2022-11-22-wasm-backend-merged-in-ghc/)
into GHC as well (great work and congrats!). WebAssembly is not as ubiquitous as
JavaScript yet, and has a harder time interacting with JavaScript directly.
Hence, we believe that both backends provide different strengths, and it is to
our benefit, and the community's, to back both code generation paths in GHC for
different use cases and requirements.

## Why Haskell?

Other languages such as [PureScript](https://www.purescript.org/) target
Javascript and provide a programmer experience close to Haskell's. The benefit
of using Haskell instead is code sharing: it becomes possible to have a web app in which the frontend is Haskell code compiled to
JavaScript and the backend is Haskell code compiled to machine code. In particular
the (de)serialization code (e.g. from/to JSON) is shared and cannot get out of
sync between the frontend and the backend.

A lot of IOG code is written in Haskell. Replicating parts of this code in
other languages would be a maintenance burden. That's why we invested in
this JavaScript backend for GHC.

## Why a GHC backend?

Haskell is a language driven by its implementation in GHC. GHC development is
very active and the GHC API is totally unstable. As a consequence, maintaining a
Haskell to Javascript compiler based on GHC but outside of GHC is costly.

The maintenance burden is not hypothetical; our teammate Luite Stegeman has been
developing a fork of GHC that emits JavaScript, called GHCJS, for close to 10 years and has experienced the pain first hand.
GHCJS relied on a modified fork of the GHC library to suit GHCJS's needs. So any
changes to upstream GHC had to be adapted to the customized fork or GHCJS would
fall behind. And fall behind it did: at the time of writing, GHCJS has stuck to using GHC
8.10, lagging behind by three major releases and counting.

Compounding the issue, the normal Haskell toolchain was not designed for an
edge case like GHCJS. So GHCJS required that the normal tooling, e.g., Cabal and
Stack, could distinguish between upstream GHC library code and GHCJS code. This
meant that the GHCJS developers had to maintain the GHC fork, develop GHCJS, and
patch or contribute to Cabal and Stack. Simply put, the maintenance burden was
much too high per developer.

So instead of spending engineering time and energy _responding_ to ecosystem
changes and maintenance, the DevX team decided the best course of action was to
enhance GHC's cross-compilation support and add a proper JavaScript backend
based on GHCJS. We feel that this adds value to the entire Haskell ecosystem,
keeps the JavaScript backend in sync with GHC, provides a better user experience
for all, reduces maintenance costs, and greatly improves the backends in GHC in
general. By implementing support for a JavaScript backend in GHC, we also
improve GHC's support for cross-compilation (and testing cross-compilers), which
is directly applicable to the WebAssembly, iOS, and Android backends in GHC.

## Is GHCJS dead?

Not yet! As it stands, the JavaScript backend doesn't provide all the features
provided by GHCJS. In particular it doesn't support Template Haskell and we've
removed the GHCJS FFI to refine its design. See our roadmap below for more
details.

Nevertheless GHCJS is unlikely to be updated to use a GHC version more recent
than 8.10.x. So from our point of view it is in maintenance mode until the
JavaScript backend totally subsumes its features. However it's an open source
project so feel free to offer patches to update it to use newer version of the
GHC library.


## What is missing from GHCJS?

The JavaScript backend borrows a lot of code from GHCJS, but not all of it. If
you are a GHCJS user, here are the main differences:

1. GHCJS was stuck to GHC 8.10 while the JavaScript backend follows GHC head.

2. GHCJS's incremental linking support ("base" bundles) hasn't been ported. This
   feature required too many changes (such as adding new command-line flags) and
   would have been backend-specific. This might be implemented in the future if
   it proves to be useful for the newer Template Haskell implementation, for example.

3. GHCJS's JavaScript code optimizer hasn't been ported. The code was trying to
   do too much all at once and consequently was fragile and slow. We plan to
   work on an intermediate representation between STG and JavaScript to perform
   the same optimizations with better performance, maintainability, and
   reliability.

4. GHCJS's compactor (link time optimizations) code hasn't been ported. Some
   optimizations have been reimplemented (e.g. global renaming of local
   identifiers), but some other are lacking (e.g. compacting initialization code).
   We plan to work on this as part of a larger effort on refactoring the code
   generator, the linker, and some aspects of the runtime system.
   More details are available in [GHC issue #22352](https://gitlab.haskell.org/ghc/ghc/-/issues/22352).

5. GHCJS's support for plugins hasn't been ported. The implementation was
   unsafe (exchanging a unit for another when GHC tries to load a plugin).
   Fixing this properly is a daunting task because of GHC's lack of modularity
   (see [#14335](https://gitlab.haskell.org/ghc/ghc/-/issues/14335)): GHC assumes there is a single global unit environment.
   We implemented a more principled workaround in
   [#20964](https://gitlab.haskell.org/ghc/ghc/-/issues/20964) /
   [!7377](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/7377) that works
   in any GHC cross-compiler.

6. GHCJS's support for Template Haskell hasn't been ported. GHCJS had its own implementation
   of an external interpreter (THRunner) which has been used as an inspiration
   to implement GHC's external interpreter (IServ). However, both
   implementations are directly incompatible. Retrofitting THRunner into Iserv
   is our next priority. More details on
   https://engineering.iog.io/2022-05-17-javascript-template-haskell-external-interpreter

7. GHCJS supported an extended FFI import syntax allowing Javascript code to be
   inlined (the FFI import string supports templates of Javascript code with
   placeholders for arguments). This hasn't been ported because adding a
   JavaScript parser to GHC wasn't trivial, and the imported code made no safety
   guarantees whatsoever. For now, only JavaScript function calls are supported.

8. Any command-line flag introduced by GHCJS has not been ported. We didn't make
   any change to GHC's command line in this work except for adding a `-ddump-js`
   flag. Other options will be added later as needed.

9. The JavaScript backend itself hasn't been optimized and we even removed some
   (seemingly random) uses of NFData from GHCJS's code. We intend to optimize
   the JavaScript backend in a principled way (e.g. by first gathering evidence
   with ticky profiles, etc.).


## What's on the JS backend roadmap?

Our top priorities are:

- Implementing Template Haskell support
- Reducing generated JavaScript code size
- Modernizing the generated JavaScript code
- Enhancing the run-time performance of the generated code

## What has improved compared to GHCJS?

Or, why did it take you so long to port a stripped GHCJS into GHC? While it may
seem like such a task should be relatively quick&mdash;especially in a language
with such a good refactoring story like haskell&mdash;there were numerous road
blocks that we needed to remove before adding the backend. In particular, here
were the troublesome bits:

#### Removing the use of external libraries

GHCJS used libraries that aren't already dependencies of GHC, such as `text`, `lens`,
`megaparsec`, and `aeson`. As we didn't want to add new dependencies to GHC, we've
refactored the code to avoid them. Examples:

- we've replaced `Text` with GHC's `ShortText` (which provides a similar API) and
  finally with GHC's FastString in most cases (which is usually more performant)
- we've replaced a lot of lens-heavy code with its non-lens equivalents, because GHC
  does not use lenses itself and a design requirement was to stay within
  existing code conventions.
- we've replaced pretty with GHC's pretty-printer (SDoc, etc.)
- we've replaced binary with GHC's Binary instances

GHCJS used to provide its own base and prim libraries: ghcjs-base and
ghcjs-prim. We've merged those into the existing base and ghc-prim libraries.

#### Reusing GHC's build system: Hadrian

GHCJS is known to be complex to build, relying on custom build scripts to deal
with the GHC fork it uses, etc. The JavaScript backend however is as easy to
build as any other GHC. It doesn't require any wrapper script, only the
"emconfigure" tool provided by the Emscripten project.

With a fresh checkout of the GHC source tree, you can now build a GHC with the JavaScript backend with just these commands:

> ./boot
> emconfigure ./configure --target=js-unknown-ghcjs
> ./hadrian/build --bignum=native -j

Note that if this doesn't work, up to date instructions and troubleshootings can
be found on https://gitlab.haskell.org/ghc/ghc/-/wikis/javascript-backend

The Hadrian build system has been adapted to support Cabal's `js-sources`
stanzas that are to support user-provided `.js` files. Both the `rts` and `base`
packages required this feature.

#### Support for running GHC's test suite

We can now run GHC's testsuite with the JavaScript backend enabled! We had to
tweak Hadrian to make this possible (support for cross-compilers is subpar), but
the testsuite already found some bugs that we have since fixed.

However, in order to merge for the GHC 9.6 release we had to disable many tests
because of missing features (TH, HPC, compact regions, etc.) or because the
generated code would time out (not surprising given the missing optimizer and
compactor).

But in the process of disabling those tests we've laid a good path forward.
We've added more precise properties to the testsuite which indicate the required
features to run each test. So when we implement some feature, it will be
painless to reenable all its tests. In addition, failing tests now have proper
tickets in GHC's GitLab.

We've spent some time trying to run the testsuite on CI. Sadly Hadrian doesn't
support this yet (more concretely, it doesn't properly support running the
testsuite for a cross-compiler in a bindist specified with `--test-compiler`).
Hopefully it should get fixed soon so that we can safely accept new
contributions. For the time being, the following command should run the
testsuite locally:

> ./hadrian/build --bignum=native -j2 test


#### Upgrading from GHC 8.10 to GHC 9.6

The latest version of GHCJS is based on a fork of GHC 8.10.7. We spent a
significant amount of time adapting the code generator to support GHC head. In
practice it meant:
  - adding support for new primops, especially sized primitives
  - adapting to ghc-bignum changes
  - adapting to internal changes
  - fixing support for polymorphism in kinds
  - fixing support for unlifted newtypes
  - fixing support for unboxed sums
  - many other fixes...

#### Fixing some performance issues

As we haven't ported GHCJS's Compactor, output size was predictably incredibly
large. So we've spent time reimplementing a crucial piece of the
Compactor&#151renaming and shortening of local variables&#151using a different
approach which ended up being faster than GHCJS's compactor. For the GHC devs
out there, we replaced a FastString with its FastString unique, so it was only
made possible after removing Text and switching from Text to FastString.

#### Removal of custom file extensions and support for JavaScript pragmas

GHCJS used the `.js.pp` file extension to identify JavaScript files that needed
to be passed through CPP before being valid JavaScript. Adding support for this
extension in both Hadrian and GHC proved to be more painful than just adding
support for JavaScript pragmas. So we decided to do the latter; similarly to
Haskell extension pragmas, you can now write "//#OPTIONS: CPP" in your
JavaScript files to enable the CPP pass, and the file extension is always `.js`.

While we're on the topic of file extensions, technically `.js` files don't have
to be compiled into `.o` files (contrary to C/C++/Haskell/etc. files) at all.
However, build systems (Hadrian, Cabal...) and compilers (GHC) expect this. So
for consistency with other backends, we've added a "compilation" pass for `.js`
files too. They are now renamed into `.o` files with a "//JAVASCRIPT" header
added to distinguish them from object files produced by the JavaScript backend
(and from Emscripten, in the future).

#### Cleanup and documentation {#blogs}

GHC provides some utilities (pretty-printer, binary serialization, string
interning, etc.) that GHCJS did not make use of. So we adapted the GHCJS code to
exploit these utilities, keep the JavaScript backend similar to other backends,
and for better performance.

Three of us (out of four) were totally new to GHCJS's code base.
We strived to grok the code and to make it understandable by adding
a lot of comments and refactoring. 
Throughout this process we logged our learning in our engineering blog
to explain some (sadly not all) technical details about GHCJS's internals:

- https://engineering.iog.io/2022-05-17-javascript-template-haskell-external-interpreter/
- https://engineering.iog.io/2022/05/24/april-GHCJS-Objectable-vs-GHC-Binary/
- https://engineering.iog.io/2022-07-18-lightweight-threads-on-JavaScript/
- https://engineering.iog.io/2022-07-20-js-backend-prim-types/
- https://engineering.iog.io/2022-07-26-the-ghcjs-linker/
- https://engineering.iog.io/2022-08-18-js-backend-ffi/
- https://engineering.iog.io/2022-09-23-ghcjs-heap-representation/

There are more in preparation.

#### Plugin support in cross-compilers

GHC doesn't support plugins when built as a cross-compiler (cf
[#14335](https://gitlab.haskell.org/ghc/ghc/-/issues/14335)). This is because it
isn't modular enough to support two environments at once: one for the target
code (JavaScript code here) and one for the host (e.g. native x86 or AArch64
code for the plugin). We've spent a lot of time making it more modular (see the
[Modularizing GHC](https://hsyl20.fr/home/files/papers/2022-ghc-modularity.pdf)
white paper we published earlier this year and Sylvain's [lightning
talk](https://youtu.be/OHGH5HLOCEM) at HIW 2022) but there is a lot more to do
to achieve this (cf
[#17957](https://gitlab.haskell.org/ghc/ghc/-/issues/17957)).

GHCJS used a fragile hack to support plugins: at plugin loading time it would
substitute the plugin unit with another corresponding one from another
package database.
It was fragile because it could violate GHC's single environment assumptions.

GHCJS's hack did not get ported. Nevertheless we have implemented a new way for
GHC to load plugins directly from libraries instead of packages
([#20964](https://gitlab.haskell.org/ghc/ghc/-/issues/20964)/[!7377](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/7377)).
This method doesn't require GHC to load module interfaces for the plugin and its
dependencies, hence workarounds GHC's limitations.

## What about libraries using C sources?

Libraries that use C sources (`c-sources` Cabal stanza) aren't supported by the
JavaScript backend. In the future we plan to use Emscripten to compile C sources
and then to generate some adapter code for them, but this isn't done yet.

For now, there are two ways to fix libraries that use C sources.
The C code can either be rewritten in Javascript, or it can be rewritten in
Haskell.
Then it is possible to use Cabal predicates (e.g. `arch(js)`) to select between
the different versions.

We do have a preference for writing pure Haskell versions because it is more
future proof.
For example if someone adds some new backends for Lua, Java, CLR, etc. then the
Haskell version can be directly compiled by that backend and there is no extra work.
In contrast, if the C source is rewritten in JavaScript, then it would need to
be rewritten _for each_ backend.

That is the approach we've taken when we wrote the ghc-bignum library.
Ghc-bignum provides a "native" implementation written in Haskell that is
functionally equivalent to the GMP based implementation. Of course, besides
being more future proof the Haskell version is just more pleasant to write than
the Javascript version.

Note that GHCJS came with a "shim" library where a shim is JavaScript source
code specifically for some package. For example, GHCJS provided shims for
packages like `text`, `process`, and `hashable`. We do not intend the JavaScript
backend to provide shims so these JavaScript sources will have to be upstreamed
or reimplemented in Haskell.

Note that the linking behavior is different due to the interpreted nature of
Javascript. In the JavaScript backend, we can link with libraries using foreign
imports _even if_ the imported functions don't exist. Instead of failing at link
time (which is what usually happens with native code) a JavaScript exception is
raised only when and if the imported function is called.

## How to help?

We have now reached our first milestone; anyone can easily build and test the
JavaScript backend, and anyone can open bug reports or offer patches for the
JavaScript backend on GHC's GitLab.

For those who offered their help this year: thank you! Until now it was
difficult to split the work into independent tasks (one fix led to a new
failure, which led to an architectural issue, etc.) and it was difficult to
coordinate with people outside of our team. However, we're now in a much better
position to discuss suggestions and to test/review patches in the spirit of open
source.

## tl;dr Just tell me how to say hello world {#build}

You need:

  - [emscripten](https://emscripten.org/docs/getting_started/downloads.html)
    version 3.14 or better. Be sure that is has either LLVM 15 or an up to date,
    patched LLVM 14.
  - [Nodejs](https://nodejs.org/en/), latest stable version. Only if you want to
    run the compiled JavaScript with node.
  
Most Linux distributions will have the necessary LLVM patches. If you're on NixOS,
you'll need to use `llvm_git` and hope for the best. [This
fork](https://github.com/doyougnu/ghc.nix) of `ghc.nix` will also be useful to
you.

#### checkout the GHC source
```
git clone --recurse-submodules https://gitlab.haskell.org/ghc/ghc.git
cd ghc # ensure you are in the ghc source tree for the following commands
```

#### update the submodules
```
git submodule update --init --recursive
```

#### Boot and configure for JavaScript
```
./boot && emconfigure ./configure --target=js-unknown-ghcjs
```

You should see `configure` finish and report something similar:
```
----------------------------------------------------------------------
Configure completed successfully.

   Building GHC version  : 9.5.20220819
          Git commit id  : 08c3c4783c72d3173d79ccda2ac282e2d3e04e34

   Build platform        : x86_64-unknown-linux
   Host platform         : x86_64-unknown-linux
   Target platform       : js-unknown-ghcjs

   Bootstrapping using   : /nix/store/4bkmkc7c98m4qyszsshnw9iclzzmdn4n-ghc-9.2.3-with-packages/bin/ghc
      which is version   : 9.2.3
      with threaded RTS? : YES

   Using (for bootstrapping) : /nix/store/yzs8390walgk2rwl6i5li2g672hdn0kv-gcc-wrapper-11.3.0/bin/cc
   Using clang               : /nix/store/p894nlicv53firllwgrfxfi51jzckh5l-emscripten-3.1.15/bin/emcc
      which is version       : 15.0.0
      linker options         : 
   Building a cross compiler : YES
   Unregisterised            : NO
   TablesNextToCode          : YES
   Build GMP in tree         : NO
   hs-cpp       : /nix/store/p894nlicv53firllwgrfxfi51jzckh5l-emscripten-3.1.15/bin/emcc
   hs-cpp-flags : -E -undef -traditional -Wno-invalid-pp-token -Wno-unicode -Wno-trigraphs
   ar           : /nix/store/p894nlicv53firllwgrfxfi51jzckh5l-emscripten-3.1.15/bin/emar
   ld           : /nix/store/p894nlicv53firllwgrfxfi51jzckh5l-emscripten-3.1.15/bin/emcc
   nm           : /nix/store/0dp0bfg9sncg7bjy389zwyg2gskknm6b-emscripten-llvm-3.1.15/bin/llvm-nm
   objdump      : /nix/store/zgvxnf9047rdd8g8kq2zxxm9k6kfqf8b-binutils-2.38/bin/objdump
   ranlib       : /nix/store/p894nlicv53firllwgrfxfi51jzckh5l-emscripten-3.1.15/bin/emranlib
   otool        : otool
   install_name_tool : install_name_tool
   windres      : 
   dllwrap      : 
   genlib       : 
   Happy        : /nix/store/ijdmyaj6i6hgx5ll0lxxgcm9b0xn8nma-happy-1.20.0/bin/happy (1.20.0)
   Alex         : /nix/store/qzgm2m7p7xc0fnyj4vy3jcmz8pvbg9p7-alex-3.2.6/bin/alex (3.2.6)
   sphinx-build : /nix/store/27dk5i52465a4azjr2dqmrhyc0m4lpf2-python3.9-sphinx-4.5.0/bin/sphinx-build
   xelatex      : /nix/store/8jc2258h4nqzqjy303zzkssd3ip675pf-texlive-combined-2021/bin/xelatex
   makeinfo     : /run/current-system/sw/bin/makeinfo
   git          : /nix/store/vsr2cn15h7cbwd5vqsam2ab2jzwfbyf9-git-2.36.0/bin/git
   cabal-install : /nix/store/cjmd2qv1b5pdw4lxh1aw4xwwy4ibnb2p-cabal-install-3.6.2.0/bin/cabal

   Using LLVM tools
      clang : clang
      llc   : llc
      opt   : opt

   HsColour was not found; documentation will not contain source links

   Tools to build Sphinx HTML documentation available: YES
   Tools to build Sphinx PDF documentation available: YES
   Tools to build Sphinx INFO documentation available: YES
----------------------------------------------------------------------
```

Be sure to verify that `ar`, `ld`, `nm` and friends point to the emscripten
versions, i.e., the output shows `<tool> : <some-path>-emscripten-<tool>`.

#### Build the JavaScript backend
```
./hadrian/build --bignum=native -j
```

#### Now compile hello world
```hs
module Main where

main :: IO ()
main = putStrLn "Hello JS!"
```

```
$ <path-to-ghc-root-dir>/_build/ghc-stage1 -fforce-recomp Main.hs
$ ./Main
$ Hello JS!
```

Under the hood `Main` is just a JavaScript program written as a script with
`nodejs` as the interpreter. This means you can treat the compiled program like
any other JavaScript program: loading it into JavaScript tooling or hack on it
by hand. This also means that all compiled programs, such as `Main`, are
human-readable, for example here are the first ten lines:

```js
$ head Main
#!/usr/bin/env node
var h$currentThread = null;
var h$stack = null;
var h$sp = 0;
var h$initStatic = [];
var h$staticThunks = {};
var h$staticThunksArr = [];
var h$CAFs = [];
var h$CAFsReset = [];
var h$regs = [];
```

The program begins with a shebang instructing the operating system to send the
rest of the file to nodejs. The remaining lines are our actual program, which
starts with global variables that the runtime system, garbage collector, and
scheduler need. Now human-readable is not the same as easy to understand, for
example here is the logic that implements a `Maybe`:

```js
function h$baseZCGHCziMaybeziJust_con_e() { return h$rs() };
function h$baseZCGHCziMaybeziJust_e() {
var h$$13be2042 = h$r2;
h$r1 = h$c1(h$baseZCGHCziMaybeziJust_con_e, h$$13be2042);
return h$rs();
};
function h$baseZCGHCziMaybeziNothing_con_e() { return h$rs() };
```

Notice that all identifiers are
[z-encoded](https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler/symbol-names)
just as they are in native GHC. For an more information on how the JavaScript
backend works please see [our other blog posts](#blogs) In any case, we invite
you to try it out, hack, and be merry!


## Acknowledgements
We want to thank Jan Hrcek, Moritz Angermann, and David Thrane Christansen for
time, labor, comments, and suggestions on drafts of this blog post.
