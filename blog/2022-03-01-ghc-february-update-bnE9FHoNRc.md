---
slug: ghc-february-update
title: GHC February Update
authors: []
tags: [ghc]
custom_edit_url: null
---
## Cross-compilation

* Ticket about adapting GHCJS’ code into a proper JS backend for GHC has been opened \[[#21078](https://gitlab.haskell.org/ghc/ghc/-/issues/21078)\] as well as the first merge requests \[[!7573](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/7573), [!7585](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/7585), [!7577](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/7577)\]
* MR status
  * ghc-proposal for OpaqueRef with related MR !7577 adding Opaque# prim type and OpaqueRep RuntimeRep. This began and focused discussions around the design of RuntimeRep for new backends.
  * js-codegen: passing CI; needs polishing
  * deriveConstants: ready
* process discussion with WT:
  * targeting GHC 9.6 (link to Matt’s mail in ghc-devs)
  * wiki page
* welcome by the community
  * people offering to help on IRC and in the ticket
  * ticket responses (emojis)

## Reporting

* we wrote a blog post about the work we have done in 2021 as it wasn’t covered anywhere else.
