---
slug: 2022-03-01-2021-ghc-update
title: 2021 GHC update
authors: []
tags: [ghc]
custom_edit_url: null
---
IOG is committed to improving Haskell developer experience, both by [sponsoring the Haskell Foundation](https://iohk.io/en/blog/posts/2020/11/04/iohk-sponsors-new-haskell-foundation) and by directly founding a team committed to this task: the Haskell DX team.

Starting now, the team will try to provide regular (monthly) updates about its work. This post is a bit longer because it covers all of 2021 which has not been covered anywhere else.

## Code generation

* Added a new backend for AArch64 architectures, especially to support Apple’s M1. Previously AArch64 was only supported via the LLVM based backend which is much slower. \[[!5884](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/5884)\]
* Added support for Apple’s M1 calling convention. In GHC 9.2.1 it implied making lifted sized types (e.g. `Word8`, `Int16`...) use their unlifted counterparts (e.g. `Word8#`, `Int16#`...); in GHC 8.10.7 – a minor release –  a less invasive but more fragile solution was implemented \[[commit](https://gitlab.haskell.org/ghc/ghc/-/commit/c49250d88915db6acf88d2574db827cc2c4fa080)\].
* Fixed a very old GHC issue \[[#1257](https://gitlab.haskell.org/ghc/ghc/-/issues/1257)\] by making GHCi support unboxed values \[[!4412](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/4412)\]: ByteCode is now generated from STG instead of directly from Core. It allows more Haskell codes to be supported by HLS and it even allows GHC code to be loaded into GHCi \[[link](https://mail.haskell.org/pipermail/ghc-devs/2021-October/020345.html)\].
* Fixed a bug in the Cmm sinking pass that led to register corruption at runtime with the C backend. Even if we don’t use the C backend, fixing this avoided spurious errors in CI jobs using it \[[#19237](https://gitlab.haskell.org/ghc/ghc/-/issues/19237),[!5755](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/5755/)\]
* Fixed a register clobbering issue for 64-bit comparisons generated with the 32-bit x86 NCG backend \[[commit](https://gitlab.haskell.org/ghc/ghc/-/commit/ecd6d14215eb40ac441c075e432ddaa0237f3c72)\].
* Fixed generation of switches on sized literals in StgToCmm \[[!6211](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/6211)\]
* Fixed LLVM shifts \[[#19215](https://gitlab.haskell.org/ghc/ghc/-/issues/19215),[!4822](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/4822)\]

## Linker

* Fixed an off-by-one error in the MachO (Darwin) linker \[[!6041](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/6041/)\]. The fix is simple but the debugging session was epic!
* Fix to avoid linking plugin units unconditionally with target code, which is wrong in general but even more so when GHC is used as a cross-compiler: plugins and target code aren’t for the same platform \[[#20218](https://gitlab.haskell.org/ghc/ghc/-/issues/20218),[!6496](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/6496)\]

## Cross-compilation

* With John Ericson (Obsidian Systems) we finally made GHC independent of its target \[[!6791](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/6791),[!6539](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/6539)\]. It means that there is no need to rebuild GHC to make it target another platform, so it now becomes possible to add support for a `--target=...` command-line flag \[[#11470](https://gitlab.haskell.org/ghc/ghc/-/issues/11470)\]. It also means that a cross-compiling GHC could build plugins for its host platform in addition to building code for its target platform.
* A side-effect of the previous bullet is that primops’ types are now platform independent. Previously some of them would use Word64 on 32-bit architectures and Word on 64-bit architectures: now Word64 is used on every platform. A side-effect of this side-effect is that we had to make Word64 as efficient as Word: it now benefits from the same optimizations (constant folding [#19024](https://gitlab.haskell.org/ghc/ghc/-/issues/19024), etc.). On 32-bit platforms, it reduced allocations by a fair amount in some cases: e.g. -25.8% in T9203 test and -11.5% when running haddock on base library \[[!6167](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/6167)\]. We hope it will benefit other 32-bit architectures such as JavaScript or WebAssembly.
* GHC built as a cross-compiler doesn’t support compiler plugins \[[#14335](https://gitlab.haskell.org/ghc/ghc/-/issues/14335)\]. We have been working on refactoring GHC to make it support two separate environments in a given compiler session – one for target code and another for the plugin/compiler code. The implementation in \[[!6748](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/6748)\] conflicts quite a lot with the support of multiple home-units that was added at about the same time. GHC needs to be refactored a lot more to correctly support this approach, so instead we implemented a different approach to load plugins which is more low-level and bypasses the issue \[[#20964](https://gitlab.haskell.org/ghc/ghc/-/issues/20964), [!7377](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/7377)\].
* We made GHC consider the target platform instead of the host platform in guessOutputFile \[[!6116](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/6116)\]
* Use target platform instead of host platform to detect literal overflows \[[#17336](https://gitlab.haskell.org/ghc/ghc/-/issues/17336),[!4986](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/4986)\]

## GHCJS

* We updated GHCJS to use GHC 8.10.7 \[[branch](https://github.com/ghcjs/ghcjs/tree/ghc-8.10)\]
* We worked on making GHCJS’s codebase more suitable for integration into GHC: reducing the number of dependencies, avoiding the use of Template Haskell, reusing GHC’s build system, etc. There is now a GHCJS integrated into a GHC 8.10.7 fork \[[branch](https://github.com/ghcjs/ghc/tree/ghc-8.10-ghcjs)\].
* This experience led us to plan the realization of a JS backend into GHC head based on GHCJS. More information about this topic in our next report.
* We worked on making GHC’s testsuite pass with GHCJS, triaging tests that legitimately fail on a JS platform from tests revealing real GHCJS issues. **\[LINK\]**

## Windows

* We seemed to be the first to try to build GHC on Windows with the updated GNU autotools 2.70 and this release made a breaking change to the way auxiliary files (config.guess, config.sub) were handled, breaking GHC’s build ([#19189](https://gitlab.haskell.org/ghc/ghc/-/issues/19189#note_332168)). The root cause of the issue couldn’t be easily solved so we modified GHC’s build system to avoid the use of these auxiliary files, bypassing the issue. Most GHC devs won’t ever notice that something was broken to begin with when they will update their GNU toolchain on Windows. \[[!4768](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/4768),[!4987](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/4987),[!5065](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/5065/)\]
* Fixed cross-compilation of GHC from Linux to Windows using Hadrian \[[#20657](https://gitlab.haskell.org/ghc/ghc/-/issues/20657),[!6945](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/6945),[!6958](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/6958)\]

## Numeric

* Fixed Natural to Float/Double conversions to align with the method used for Integer to Float/Double and added missing rewrite rules \[[!6004](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/6004/)\]
* Made most bignum literals be desugared into their final form in HsToCore stage instead of CoreToStg stage to ensure that Core optimizations were applied correctly to them \[[#20245](https://gitlab.haskell.org/ghc/ghc/-/issues/20245),[!6376](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/6376)\]
* Some constant folding rules were missing and were added:
  * bitwise `and` primops when applied to a full mask (e.g. 0xFF for a 8-bit word). \[[#20448](https://gitlab.haskell.org/ghc/ghc/-/issues/20448),[!6629](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/6629)\]
  * `negate` primops [#20347](https://gitlab.haskell.org/ghc/ghc/-/issues/20347),[!6535](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/6535)
  * `timesInt2#` primop [#20374](https://gitlab.haskell.org/ghc/ghc/-/issues/20374),[!6531](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/6531)
  * `ctz#/clz#/popCnt#` [#20376](https://gitlab.haskell.org/ghc/ghc/-/issues/20376),[!6532](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/6532)
  * missing rewrite rule to make the implementation of `nat2Word#` efficient \[[#15547](https://gitlab.haskell.org/ghc/ghc/-/issues/15547),[!6847](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/6847)\]
  * rules for `Natural` \[[#15821](https://gitlab.haskell.org/ghc/ghc/-/issues/15821),[!4837](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/4837)\]
* Allowed some ghc-bignum operations to inline to get better performance, while still managing to keep constant-folding working \[[#19641](https://gitlab.haskell.org/ghc/ghc/-/issues/19641),[!6677](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/6677),[!6696](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/6696),[!6306](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/6306)\]. There is some work left to do (cf [#20361](https://gitlab.haskell.org/ghc/ghc/-/issues/20361)) but it is blocked by [#19313](https://gitlab.haskell.org/ghc/ghc/-/issues/19313) which in turn is blocked by [#20554](https://gitlab.haskell.org/ghc/ghc/-/issues/20554) which should be fixed soon ([!6865](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/6865), thanks Joachim!).
* The ubiquitous `fromIntegral` function used to have many associated rewrite rules to make it fast (avoiding heap allocation of a passthrough Integer when possible) that were difficult to manage due to the combinatorial number of needed rules ([#19907](https://gitlab.haskell.org/ghc/ghc/-/issues/19907), [#20062](https://gitlab.haskell.org/ghc/ghc/-/issues/20062)). We found a way to remove all these rules ([!5862](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/5862)).

## Technical debt & modularity

* Made several component of the compiler independent of `DynFlags` (parsed command-line flags):
  * TmpFS (dealing with temporary files) \[[!6186](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/6186)\]
  * Diagnostic options \[[!6043](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/6043)\]
  * Tracing functions \[[!5970](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/5970)\]
  * Logger \[[!4757](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/4757)\]
  * Logger & Parser \[[!5845](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/5845)\]
  * Hooks \[[!4812](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/4812)\]
* Made the handling of “package imports” less fragile \[[!6586](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/6586)\] and refactored some code related to dependencies and recompilation avoidance \[[!6528](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/6528),[!6346](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/6346)\].
* Abstracted plugin related fields from HscEnv \[[!7175](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/7175)\]
* Made a home-unit optional in several places \[[!7013](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/7013/)\]: the home-unit should only be required when compiling code, not when loading code (e.g. when loading plugins in cross-compilers [#14335](https://gitlab.haskell.org/ghc/ghc/-/issues/14335)).
* Made GHC no longer expose the (wrong) selected ghc-bignum backend with `ghc --info`. ghc-bignum now exposes a backendName function for this purpose \[[#20495](https://gitlab.haskell.org/ghc/ghc/-/issues/20495),[!6903](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/6903)\]
* Moved `tmpDir` from Settings to `DynFlags` \[[!6297](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/6297/)\]
* Removed use of `unsafePerfomIO` in `getProgName` \[[!6137](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/6137/)\]
* Refactored warning flags handling \[[!5815](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/5815)\]
* Made assertions use normal functions instead of CPP \[[!5693](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/5693)\]
* Made the interpreter more independent of the driver \[[!5627](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/5627)\]
* Replaced `ptext . sLit` with `text` \[[!5625](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/5625)\]
* Removed broken “dynamic-by-default” setting \[[#16782](https://gitlab.haskell.org/ghc/ghc/-/issues/16782),[!5467](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/5467)\]
* Abstracted some components from the compiler session state (`HscEnv`):
  * unit-related fields into a new `UnitEnv`datatype \[[!5425](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/5425)\]
  * `FinderCache` and `NameCache`\[[!4951](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/4951)\]
  * Loader state \[[!5287](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/5287)\]
* Removed the need for a home unit-id to initialize an external package state (EPS) \[[!5043](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/5043)\]
* Refactored `-dynamic-too` handling \[[#19264](https://gitlab.haskell.org/ghc/ghc/-/issues/19264),[!4905](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/4905)\]

## Performance

* Made `divInt#, modInt# and divModInt#` branchless and inlineable \[[#18067](https://gitlab.haskell.org/ghc/ghc/-/issues/18067),[#19636](https://gitlab.haskell.org/ghc/ghc/-/issues/19636),[!3229](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/3229)\]
* Fixed Integral instances for Word8/16/32 and `showWord` to use `quotRemWordN` \[[!5891](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/5891),[!5846](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/5846/)\]
* Improved performance of occurrence analysis \[[#19989](https://gitlab.haskell.org/ghc/ghc/-/issues/19989),[!5977](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/5977)\]
* Fixed unnecessary pinned allocations in `appendFS` \[[!5989](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/5989/)\]
* Added a rewrite rules for string literals:
  * Concatenation of string literals \[[#20174](https://gitlab.haskell.org/ghc/ghc/-/issues/20174),[#16373](https://gitlab.haskell.org/ghc/ghc/-/issues/16373),[!6259](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/6259)\]
  * `(++) . unpackCString# ⇒ unpackAppendCString#` leading to a 15% reduction in compilation time on a specific example. \[[!6619](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/6619)\]
  * Compute SDoc literal size at compilation time \[[#19266](https://gitlab.haskell.org/ghc/ghc/-/issues/19266), [!4901](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/4901)\]
* Fix for Dwarf strings generated by the NCG that were unnecessarily retained in the FastString table \[[!6621](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/6621)\]
* Worked on improving inlining heuristics by taking into account applied constructors at call sites \[[#20516](https://gitlab.haskell.org/ghc/ghc/-/issues/20516),[!6732](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/6732)\]. More work is needed though.
* Fixed [#20857](https://gitlab.haskell.org/ghc/ghc/-/issues/20857) by making the Id cache for primops used more often \[[!7241](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/7241)\]
* Replaced some avoidable uses of `replicateM . length` with more efficient code \[[!7198](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/7198)\]. No performance gain this time but the next reader of this code won’t have to wonder if fixing it could improve performance.
* Made `exprIsCheapX` inline for modest but easy perf improvements \[[!7183](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/7183)\]
* Removed an allocation in the code used to write text on a Handle (used by putStrLn, etc.) \[[!7160](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/7160)\]
* Replaced inefficient list operations with more efficient `Monoid ([a],[b])` operations in the driver \[[!7069](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/7069)\], leading to 1.9% reduction in compiler allocations in MultiLayerModules test.
* Disabled some callstack allocations in non-debug builds \[[!6252](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/6252/)\]
* Made file copy in GHC more efficient \[[!5801](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/5801)\]
* Miscellaneous pretty-printer enhancements \[[!5226](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/5226)\]
* Type tidying perf improvements with strictness \[[#14738](https://gitlab.haskell.org/ghc/ghc/-/issues/14738),[!4892](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/4892)\]

## RTS

* Fixed issues related to the RTS’s ticker
  * Fixed some races \[[#18033](https://gitlab.haskell.org/ghc/ghc/-/issues/18033),[#20132](https://gitlab.haskell.org/ghc/ghc/-/issues/20132),[!6201](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/6201)\]
  * Made the RTS open the file descriptor for its timer (`timerfd`) on Linux synchronously to avoid weird interactions with Haskell code manipulating file descriptors \[[#20618](https://gitlab.haskell.org/ghc/ghc/-/issues/20618),[!6902](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/6902)\].
* Moved GHC’s global variables used to manage Uniques into the RTS to fix plugin issues \[[#19940](https://gitlab.haskell.org/ghc/ghc/-/issues/19940),[!5900](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/5900)\]

## Build system / CI

* Fixed Hadrian output to display warnings and errors after the multi screen long command lines \[[#20490](https://gitlab.haskell.org/ghc/ghc/-/issues/20490),[!6690](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/6690)\]
* Avoided the installation of a global `platformConstants` file; made GHC load constants from the RTS unit instead, allowing it to be reinstalled with different constants \[[!5427](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/5427)\]
* Made `deriveConstants` output its file atomically \[[#19684](https://gitlab.haskell.org/ghc/ghc/-/issues/19684),[!5520](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/5520)\]
* Made compression with `xz` faster on CI \[[!5066](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/5066)\]
* Don’t build extra object with `-no-hs-main` \[[#18938](https://gitlab.haskell.org/ghc/ghc/-/issues/18938),[!4974](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/4974)\]
* Add `hi-boot` dependencies with `ghc -M` \[[#14482](https://gitlab.haskell.org/ghc/ghc/-/issues/14482),[!4876](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/4876)\]

## Misc

* Stack: fixed interface reading in `hi-file-parser` to support GHC 8.10 and 9.0 \[[PR](https://github.com/commercialhaskell/hi-file-parser/pull/2), [Stack#5134](https://github.com/commercialhaskell/stack/issues/5134)\]
* Enhanced pretty-printing of coercions in Core dumps \[[!4856](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/4856)\]
