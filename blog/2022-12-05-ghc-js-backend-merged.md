---
slug: 2022-12-05-ghc-js-backend-merged
title: JavaScript backend merged into GHC
authors: [sylvain]
tags: [ghc]
---

For most of the year 2022, we, the GHC DevX team at IOG, have been working on
implementing a JavaScript backend for GHC, based on GHCJS. It has just been
[merged](https://gitlab.haskell.org/ghc/ghc/-/commit/cc25d52e0f65d54c052908c7d91d5946342ab88a)
into the master branch on November 30th, 2022. This post should answer most of
the questions you may have about this backend.

## Why Javascript?

The only reason to target Javascript is that it is (sadly) the most portable
language especially to write user interfaces.

What about WebAssembly? WebAssembly is a more promising target language for
performance than Javascript, but it isn't as portable as Javascript yet.
Moreover WebAssembly can't directly access Web APIs (DOM manipulation, etc.),
hence still needs some Javascript glue code. For our purpose (portable GUIs),
sticking to Javascript is still a better option. That being said, our friends at
[Tweag](https://www.tweag.io/) have implemented and merged a new
[WebAssembly](https://www.tweag.io/blog/2022-11-22-wasm-backend-merged-in-ghc/)
into GHC!

## Why Haskell?

Other languages such as PureScript target Javascript and provide a programmer
experience close to Haskell's. The benefit of using Haskell instead is code
sharing: frontend is Haskell code compiled to Javascript and backend is Haskell
code compiled to machine code. In particular the (de)serialization code (e.g.
from/to JSON) is shared and can't get out of sync between the frontend and the
backend.

A lot of IOG code is written in Haskell. Duplicating some of this code into some
other language(s) would be a maintenance burden. That's why we invested into
this Haskell to Javascript compiler.

## Why a GHC backend?

Haskell is a language driven by its implementation in GHC. GHC development is
very active and the GHC API is totally unstable. As a consequence, maintaining a
Haskell to Javascript compiler based on GHC but outside of GHC is costly.

Our teammate Luite Stegeman has been developing GHCJS for 10 years. Recent
releases of GHCJS have been relying on a fork of the GHC library. This fork was
modified to add and to change whatever GHCJS needed. As a result it was much
more difficult to keep up with the upstream GHC library and GHCJS was stuck to
using GHC 8.10.

Being a different project, GHCJS also required other tools (Cabal, Stack...) to
distinguish between GHC and GHCJS. It meant more maintenance burden for
everyone involved in these projects.

Enhancing GHC's cross-compilation support and adding a proper JS backend was the
best way to reduce the technical debt, to reduce future maintenance costs, and
to provide a better user experience.

## Is GHCJS dead?

Not yet! As it stands, the JS backend doesn't provide all the features provided
by GHCJS yet. In particular it doesn't support Template Haskell yet. See our
roadmap below for more details.

Nevertheless GHCJS is unlikely to be updated to use a GHC version more recent
than 8.10.x. So from our point of view it is in maintenance mode until the JS
backend totally subsumes its features. However it's an open source project so
feel free to offer patches to update it to use newer version of the GHC library.


## What is missing from GHCJS?

The JS backend borrows a lot of code from GHCJS, but not all of it. If you are a
GHCJS user, here are the main differences:

1. GHCJS was stuck to GHC 8.10 while the JS backend follows GHC head.

1. GHCJS's incremental linking support ("base" bundles) hasn't been ported. This
   feature required too many changes (e.g. adding new command-line flags) and
   would have been backend-specific. This might be implemented in the future if
   it proves to be useful for the newer TH implementation for example.

2. GHCJS's JS code optimizer hasn't been ported. The code was fragile and slow. We
   plan to work on an intermediate representation between STG and JS to perform
   the same optimizations in a safer/faster way.

3. GHCJS's compactor (link time optimizations) code hasn't been ported. Some
   optimizations have been reimplemented (e.g. global renaming of local
   identifiers), but some other are lacking (e.g. compacting initialization code).
   We plan to work on this as part of a larger effort on refactoring the code
   generator, the linker, and some runtime system aspects.
   See also [#22352](https://gitlab.haskell.org/ghc/ghc/-/issues/22352)

4. GHCJS's support for plugins hasn't been ported. The implementation was
   unsafe (exchanging a unit for another when GHC tries to load a plugin).
   Fixing this properly is a daunting task because of GHC's lack of modularity
   (see [#14335](https://gitlab.haskell.org/ghc/ghc/-/issues/14335)): GHC assumes there is a single global unit environment.
   We implemented a more principled workaround in
   [#20964](https://gitlab.haskell.org/ghc/ghc/-/issues/20964) /
   [!7377](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/7377) that works
   in any GHC cross-compiler.

5. GHCJS's support for TH hasn't been ported. GHCJS had its own implementation
   of an external interpreter (THRunner) which has been used as an inspiration
   to implement GHC's external interpreter (IServ). However both implementations
   aren't directly compatible. Retrofitting THRunner into Iserv is our next
   priority. More details on https://engineering.iog.io/2022-05-17-javascript-template-haskell-external-interpreter

6. GHCJS supported an extended FFI import syntax allowing Javascript code to be
   inlined (the FFI import string supports templates of Javascript code with
   placeholders for arguments). This hasn't been ported as adding a Javascript
   parser to GHC wasn't trivial. For now, only JS function calls are supported.

7. Any command-line flag introduced by GHCJS hasn't been ported yet. We didn't
   make any change to GHC's command-line in this work except for adding a
   `-ddump-js` flag. Other options will be added later as needed.

8. The JS backend itself hasn't been optimized and we even removed some
   (seemingly random) uses of NFData from GHCJS's code. We intend to optimize
   the JS backend in a principled way (e.g. by first getting evidences with
   ticky profiles, etc.).


## What's on the JS backend roadmap?

Our top priorities are:

- Implementing Template Haskell support
- Reducing generated JS code size
- Modernizing the generated JS code
- Enhancing runtime performance

## What has improved compared to GHCJS?

Or why did it take you so long to port a stripped GHCJS into GHC?

1. Removing the use of external libraries

GHCJS used libraries that aren't already dependencies of GHC: text, lens,
megaparsec, aeson... As we didn't want to add new dependencies to GHC, we've
refactored the code to avoid them. Examples:

- we've replaced Text with GHC's ShortText (which provides a similar API) and
  finally with GHC's FastString in most cases (which is usually more performant)
- we've replaced a lot of code using lenses with its understandable
  counterpart (which is more common in GHC)
- we've replaced pretty with GHC's pretty-printer (SDoc, etc.)
- we've replaced binary with GHC's Binary instances

GHCJS used to provide its own base and prim libraries: ghcjs-base and
ghcjs-prim. We've merged those into the existing base and ghc-prim libraries.

2. Reusing GHC's build system: Hadrian

GHCJS is known to be complex to build, relying on custom build scripts to deal
with the GHC fork it uses, etc. The JS backend however is as easy to build as
any other GHC. It doesn't require any wrapper script, only the "emconfigure"
tool provided by the Emscripten project.

You can now build a GHC with the JS backend with these commands:

> ./boot
> emconfigure ./configure --target=js-unknown-ghcjs
> ./hadrian/build --bignum=native -j

Note that if this doesn't work, up to date instructions and troubleshootings can
be found on https://gitlab.haskell.org/ghc/ghc/-/wikis/javascript-backend

Hadrian build system has been adapted to support Cabal's `js-sources`
stanzas that are to support user-provided `.js` files. `rts` and `base`
packages both require this feature.

3. Support for running GHC's testsuite

We can now run GHC's testsuite with the JS backend enabled! We had to tweak
Hadrian to make this possible (support for cross-compilers is subpar). The
testsuite already found some bugs that we have spent time fixing.

Quite a lot of tests had to be disabled because of missing features (TH, HPC,
compact regions, etc.) or because the generated code would timeout. We've added
more precise properties to test descriptions that indicate required features to
run each test. When we'll implement some feature, it will be much easier to
reenable all its tests at once. In addition failing tests now have proper
tickets in GHC's gitlab.

We've spent some time trying to run the testsuite on CI.
Sadly Hadrian doesn't support this yet (more concretely, it doesn't
properly support running the testsuite for a cross-compiler in a bindist
specified with --test-compiler).
Hopefully it should get fixed soon so that we can easily accept new
contributions.
For the time being, the following command should run the testsuite locally:

> ./hadrian/build --bignum=native -j2 test


4. Upgrading from GHC 8.10 to GHC 9.6

The latest version of GHCJS is based on a fork of GHC 8.10.7. One time consuming
task has been to adapt the code generator to support GHC head. In practice it meant:
  - adding support for new primops, especially sized primitives
  - adapting to ghc-bignum changes
  - adapting to internal changes
  - fixing support for polymorphism in kinds
  - fixing support for unlifted newtypes
  - fixing support for unboxed sums
  - many other fixes...

5. Fixing some performance issues

As we haven't ported GHCJS's Compactor, output size ended up being really too
big. We've spent some time reimplementing a very important part of the
Compactor---renaming and shortening of local variables---using a different
approach which ends up being faster than GHCJS's one. Technically we replace a
FastString with its FastString unique, so it was only made possible by the
switch from Text to FastString.

6. Removal of custom file extensions and support for JS pragmas

GHCJS used the .js.pp file extension to identify JS files that need to be passed
through CPP. Adding support for this extension in Hadrian and GHC proved to be
more painful than adding support for JS pragmas. Similarly to Haskell extension
pragmas, you can now write "//#OPTIONS: CPP" in your JS files to enable the CPP
pass, and extension is always .js.

On the topic of file extensions, technically .js files don't have to be compiled
into .o files, contrary to C/C++/Haskell/etc. files. However build systems
(Hadrian, Cabal...) and compilers (GHC) expect this. For consistency we've
added a "compilation" pass for .js files too. They are now renamed into .o
files with a "//JAVASCRIPT" header added to distinguish them from object files
produced by the JS backend (and from Emscripten, in the future).

7. General cleanup and documentation

GHC provides some utilities (pretty-printer, binary serialization, string
interning, etc.) that weren't used before.
We adapted the code to use them to make the JS backend similar to other
backends and for performance reasons.

Three of us (out of four) were totally new to GHCJS's code base.
We strived to understand the code and to make it easier to understand by adding
a lot of comments and by refactoring it.
We also wrote a few blog posts explaining some technical details about GHCJS's
internals:

- https://engineering.iog.io/2022-05-17-javascript-template-haskell-external-interpreter/
- https://engineering.iog.io/2022/05/24/april-GHCJS-Objectable-vs-GHC-Binary/
- https://engineering.iog.io/2022-07-18-lightweight-threads-on-JavaScript/
- https://engineering.iog.io/2022-07-20-js-backend-prim-types/
- https://engineering.iog.io/2022-07-26-the-ghcjs-linker/
- https://engineering.iog.io/2022-08-18-js-backend-ffi/
- https://engineering.iog.io/2022-09-23-ghcjs-heap-representation/

There are more in preparation.

8. Plugin support in cross-compilers

GHC doesn't support plugins when it is built as a cross-compiler (cf
[#14335](https://gitlab.haskell.org/ghc/ghc/-/issues/14335)).
This is because it isn't modular enough to support two environments at once: one
for the target code (JS code here) and one for the host (e.g. native x86 or
AArch64 code for the plugin).
We've spent a lot of time making it more modular (see the "Modularizing GHC"
white paper we published earlier this year and Sylvain's lightning talk at HIW 2022)
but there is a lot more to do to achieve this (cf
[#17957](https://gitlab.haskell.org/ghc/ghc/-/issues/17957)).

GHCJS used a fragile hack to support plugins: at plugin loading time it would
substitute the plugin unit with another corresponding one from another
package database.
It was fragile because it could violate GHC's single environment assumptions.

We didn't port GHCJS's hack. Nevertheless we have implemented a new way for GHC
to load plugins directly from libraries instead of packages (#20964 / !7377).
This method doesn't require GHC to load module interfaces for the plugin and its
dependencies, hence workarounds GHC's limitations.

## What about libraries using C sources?

Libraries that use C sources (`c-sources` Cabal stanza) aren't supported by the
JS backend.
In the future we plan to use Emscripten to compile C sources and then to
generate some adapter codes for them, but this isn't done yet.

For now there are two ways to fix libraries that use C sources.
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

That's basically what we've done when we wrote the ghc-bignum library which
provides a "native" implementation written in Haskell that is functionally
equivalent to the GMP based implementation.
Another advantage of the Haskell version is that it is much more pleasant to
write Haskell code than to write Javascript code.

Note that GHCJS came with a "shim" library where a shim is a JS source for some
package.
The JS backend won't provide shims so these JS sources will have to be
upstreamed or reimplemented in Haskell.

Note that due to Javascript interpreted nature, we can link with libraries using
foreign imports even if the imported functions don't exist.
Instead of failing at link time (what usually happens with native code) a JS
exception be raised only when and if the imported function is called.

## How to help?

We have now reached a point where anyone can easily build and test the JS
backend.
Anyone can now open bug reports and offer patches for the JS backend on GHC's gitlab.

A few people already offered their help this year: thank you!
Until now it was difficult to split the work into independent tasks (one fix led
to a new failure, etc.) and it was difficult to coordinate with people outside
of our team.
However we're now in a much better position to discuss suggestions and to test/review patches.