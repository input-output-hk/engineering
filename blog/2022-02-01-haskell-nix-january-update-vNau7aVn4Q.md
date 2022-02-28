---
slug: haskell-nix-january-update
title: haskell.nix January Update
authors: []
tags: [nix]
custom_edit_url: null
---
# **What Happened in Haskell.Nix?**

## **January 2022**

This month we merged some very significant improvements to the support for compiling for Android and iOS based AArch64 devices.  When the build system is also AArch64 template haskell can often be run locally.  This will make targeting mobile devices from AArch64 builders much easier.

A long running branch containing bug fixes for cross compilation to JavaScript with GHCJS was merged.  One nice feature included is better support for adding bindings to C code compiled with emscripten.  In some cases it can be as easy as adding a single JavaScript file to the package with wrappers for the C functions.

#### Changes

* Much improved AArch64 support including Template Haskell (#1316)
* Improved GHCJS and support for calling C code compiled with emscripten (#1311)
* The environment variables LANG and LOCALE_ARCHIVE are no longer set in shells allowing the users prefered settings to persist (#1341).
* source-repo-override argument added for cabal projects to allow the location of source-repository-package packages to be replaced (#1354)

#### Version Updates

* GHC 9.0.2 was added to the available GHC versions (#1338)
* The nixpkgs pins for 21.05, 21.11 and unstable were all updated (#1334).
* Remaining uses of cabal 3.4 were updated to 3.6.2 (#1328)

#### Bug fixes

* Dwarf build of ghc 9.2.1 now skipped on hydra to work around 4GB hydra limit (#1333)
* Removed use of propagatedBuildInputs in ghc derivation (#1318).
* Caching of the check-hydra CI script was fixed (#1340)
