---
slug: 2022-03-09-ghc-february-2022-update
title: GHC February 2022 Update
authors: [sylvain]
tags: [ghc,ghc-update]
custom_edit_url: null
---
## JS backend

This month we worked on adapting code from GHCJS to merge into GHC head. We also started discussing the implementation process publicly and especially with our colleagues at Well-Typed.

* Ticket about adapting GHCJS’ code into a proper JS backend for GHC has been opened \[[#21078](https://gitlab.haskell.org/ghc/ghc/-/issues/21078)\]. Feedback was very positive!
* There were discussions about the process and an agreement to target GHC 9.6 release \[[email on ghc-devs](https://mail.haskell.org/pipermail/ghc-devs/2022-February/020580.html), [wiki page](https://gitlab.haskell.org/ghc/ghc/-/wikis/javascript-backend)\]
* `deriveConstants` is a program used to generate some header file included in the rts package. While it is mainly useful for native targets, we had to make it support Javascript targets \[[!7585](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/7585)\]
* Javascript is going to be the first official target platform supported by GHC that has its own notion of managed heap objects. Hence we may need a new `RuntimeRep` to represent these values for Haskell codes interacting with JS codes via FFI. We opened [!7577](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/7577) into which we tried to make this new `RuntimeRep` non JS specific so that it could be reused for future backends targeting other managed platforms (e.g. CLR, JVM). It triggered a lot of discussions summarized in [#21142](https://gitlab.haskell.org/ghc/ghc/-/issues/21142).
* GHCJS’s code generator was ported to GHC head \[[!7573](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/7573)\]. In its current state, we can generate Javascript unoptimised code -- the optimiser hasn’t been ported yet -- by compiling a module with `-c -fjavascript`. It required many changes, not only to adapt to changes between GHC 8.10 and GHC head but also to avoid adding new package dependencies. It was also an opportunity to refactor and to document the code, which is still a work in progress.
* GHC doesn’t use any lens library, hence to port the code generator we had to replace lenses with usual record accessors. It turned out that `case` alternatives in STG lacked them because they were represented with a triple. We took the opportunity to introduce a proper record type for them  [!7652](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/7652)

## Plutus-apps JS demo

* We improved the proof of concept JavaScript library for generating Plutus transactions with a given set of constraints and lookups, exposing functionality from the `plutus-ledger-constraints` package. \[[Report](https://github.com/hamishmack/plutus-apps/blob/1f331225853f502807aab370f82ec975bdec38ee/plutus-pab/mktx/README.md)\]

## Reporting

* we wrote a blog post about the work we have done in 2021 as it wasn’t covered anywhere else: <https://engineering.iog.io/2022-03-01-2021-ghc-update>
