---
slug: 2022-04-19-ghc-march-2022-update
title: GHC March 2022 Update
authors: [sylvain]
tags: [ghc]
custom_edit_url: null
---
## JS Backend

In March the team focused on porting more GHCJS code to GHC head.

* Most of us are new to GHCJS’s codebase so we are taking some time to better understand it and to better document it as code gets integrated into GHC head.
* Development process: initially we had planned to integrate features one after the others into GHC head. However it was finally decided that features would be merged into a [wip/javascript-backend](https://gitlab.haskell.org/ghc/ghc/-/commits/wip/javascript-backend) branch first and then later merged into GHC head. After trying this approach we decided to work directly into another branch: [wip/js-staging](https://gitlab.haskell.org/ghc/ghc/-/commits/wip/js-staging) . Opening merge requests that can’t be tested against a branch that isn’t GHC head didn’t bring any benefit and slowed us too much.
* Documentation: we wrote a document comparing the different approaches to target JavaScript/WebAssembly [ https://gitlab.haskell.org/ghc/ghc/-/wikis/javascript](https://gitlab.haskell.org/ghc/ghc/-/wikis/javascript)
* RTS: some parts of GHCJS’s RTS are generated from Haskell code, similarly to code generated with the genapply program in the C RTS. This code has been ported to GHC head. As JS linking---especially linking with the RTS---will only be performed by GHC in the short term, we plan to make it generate this code dynamically at link time.
* Linker: most of GHCJS’s linker code has been adapted to GHC head. Because of the lack of modularity of GHC, a lot of GHC code was duplicated into GHCJS and slightly modified. Now that both codes have diverged we need to spend some time making them converge again, probably by making the Linker code in GHC more modular.
* Adaptation to GHC head: some work is underway to replace GHCJS’s Objectable type-class with GHC’s Binary type-class which serves the same purpose. Similarly a lot of uses of Text have been replaced with GHC’s ShortText or FastString.
* Template Haskell: GHCJS has its own TH runner which inspired GHC’s external interpreter (“Iserv”) programs. We have been exploring options to port TH runner code as an Iserv implementation. The Iserv protocol uses GADTs to represent its messages which requires more boilerplate code to convert them into JS because we can’t automatically derive aeson instances for them.
* Plugins: we have an MR adding support for “external static plugins” to GHC [!7377](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/7377). Currently it only supports configuring plugins *via* environment variables. We have been working on adding support for command-line flags instead.
* Testsuite: we have fixed GHC’s build system so that it can run GHC’s testsuite when GHC is built as a cross-compiler ([!7850](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/7850)). There is still some work to do (tracked in [#21292](https://gitlab.haskell.org/ghc/ghc/-/issues/21292)) to somehow support tests that *run* compiled programs: with cross-compilers, target programs can’t be directly executed by the host architecture.

## Misc

* [Performance book](https://github.com/haskellfoundation/tech-proposals/pull/26): some time was spent on the infrastructure (CI) and on switching the format of the book to ReStructured Text
* Modularity: some time was spent discussing GHC’s design and refactoring (c.f. [!7442](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/7442) and [#20927](https://gitlab.haskell.org/ghc/ghc/-/issues/20927)).
