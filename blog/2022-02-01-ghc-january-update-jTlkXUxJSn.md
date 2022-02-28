---
slug: ghc-january-update
title: GHC January update
authors: []
tags: [ghc]
custom_edit_url: null
---
Hopefully 2022 should be the year GHC will get a JavaScript backend without relying on GHCJS. This month the team has been busy planning the work that needs to be done to get there!

## Cross-compilation

* GHCJS has been [updated](https://github.com/ghcjs/ghc/tree/ghc-8.10-ghcjs) to reduce the gap with GHC 8.10.7 codebase to the point that GHC’s build system is used to build GHCJS
* Internal work planning for the integration of GHCJS into GHC
* A different approach to load plugins into cross-compilers has been implemented \[[#20964](https://gitlab.haskell.org/ghc/ghc/-/issues/20964), [!7377](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/7377)\]
* GHCJS has been exercised to showcase compilation of some Plutus applications

## Modularity

* A few “subsystems” of GHC have been made more modular and reusable by making them independent of the command-line flags (`DynFlags`) \[[#17957](https://gitlab.haskell.org/ghc/ghc/-/issues/17957), [!7158](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/7158), [!7199](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/7199), [!7325](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/7325)\]. This work resulted in a 10% reduction in call sites to `DynFlags` and has now removed all references to `DynFlags` up to the `CoreToStg` pass, which is almost the entire backend of GHC.

## Performance

* Jeffrey wrote a new HF [proposal](https://github.com/haskellfoundation/tech-proposals/pull/26) about writing a Haskell Optimization handbook and has started working on it
