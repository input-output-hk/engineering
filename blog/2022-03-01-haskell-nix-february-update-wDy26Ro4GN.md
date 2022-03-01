---
slug: 2022-03-01-haskell-nix-february-update
title: haskell.nix February Update
authors: []
tags: [nix]
custom_edit_url: null
---
## Documentation

* A tutorial has been added on [building DWARF-enabled executables](https://outline.zw3rk.com/share/d461004d-1f2f-4d7a-95f2-4e20acb18cac) on linux systems.  There was also a related fix for building DWARF executables in a nix shell ([#1385](https://github.com/input-output-hk/haskell.nix/pull/1385))

## Changes

* Support for external Hackage repositories was improved by [#1370](https://github.com/input-output-hk/haskell.nix/pull/1370). We can now use an extra package repository just by adding a `repository` block to the `cabal.project` file.  This makes it easy to make use of an extra hackage databases such as [hackage.head](https://ghc.gitlab.haskell.org/head.hackage/) and [hackage-overlay-ghcjs](https://github.com/input-output-hk/hackage-overlay-ghcjs).  A `sha256` for the repository it can be added as a comment in the `repository` block or by including it in the `sha256map` argument.

## Version Updates

* nix-tools was updated to use the Cabal 3.6.2 and hnix 0.16 [nix-tools#113](https://github.com/input-output-hk/nix-tools/pull/113)
* Nixpkgs pins were bumped [#1371](https://github.com/input-output-hk/haskell.nix/pull/1371)
* Update booting on aarch64 linux to ghc 8.8.4 [1325](https://github.com/input-output-hk/haskell.nix/pull/1325) and [1374](https://github.com/input-output-hk/haskell.nix/pull/1374)

## Bug fixes

* Allow linking pcre statically with musl [#1363](https://github.com/input-output-hk/haskell.nix/pull/1363)
* Add gpiod to system nixpkgs map [#1359](https://github.com/input-output-hk/haskell.nix/pull/1359)
* Add poppler-cpp to png-config Nixpkgs map [#1373](https://github.com/input-output-hk/haskell.nix/pull/1373)
* Use the same logic that cabal-install uses for determining the path of a packages `.tar.gz` in a repository  [nix-tools#114](https://github.com/input-output-hk/nix-tools/pull/114)
* Fix libnuma dependency in rts.conf [1342](https://github.com/input-output-hk/haskell.nix/commit/18ebf60137dd2ff1be7363eb46f67ebfa366d1dd)
* Fix when "materialized" dir is deep [#1376](https://github.com/input-output-hk/haskell.nix/pull/1376)
* Prefer local building for `git-ls-files` [#1378](https://github.com/input-output-hk/haskell.nix/pull/1378) and [#1381](https://github.com/input-output-hk/haskell.nix/issues/1381)
* Fix stack cache generator `sha256` is a string not a lambda [#1383](https://github.com/input-output-hk/haskell.nix/pull/1383)
* Only pass `--index-state` to `cabal` when asked [#1384](https://github.com/input-output-hk/haskell.nix/pull/1384)
* Pass `enableDWARF` to `makeConfigFiles` to fix `-g3` support in `nix-shell` [#1385](https://github.com/input-output-hk/haskell.nix/pull/1385)

Finally, we’d like to thank all the awesome contributors, who make `haskell.nix` a thriving open source project! :heart:
