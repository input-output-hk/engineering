---
slug: 2022-04-08-haskell-nix-march-update
title: haskell.nix March Update
authors: []
tags: [nix]
custom_edit_url: null
---
## Changes

* To cross compile Haskell code for windows a `wine` process must be used to evaluate Template Haskell code at compile time.  Some times this code needs DLLs to be present for the Template Haskell code to run.  We had been maintaining a list of DLLs manually ([#1400](https://github.com/input-output-hk/haskell.nix/pull/1400) for instance added `secp256k1`).  A more general solution ([#1405](https://github.com/input-output-hk/haskell.nix/pull/1405)) was found that uses the `pkgsHostTarget` environment variable to obtain a list of all the packages dependencies.  Then the DLLs from the are made available to the `wine` process running the Template Haskell code.  This should make more libraries build correctly while reducing unnecessary dependencies.
* The way Haskell.nix cleans source trees has changed with [#1403](https://github.com/input-output-hk/haskell.nix/pull/1403), [#1409](https://github.com/input-output-hk/haskell.nix/pull/1409) and [#1418](https://github.com/input-output-hk/haskell.nix/pull/1418).  When using Nix `>=2.4` source in the store is now filtered in the same way it is locally.  This has a couple of key advantages:
  * It makes it less likely that results on CI systems (where the source is likely to be in the store) will differ from results for local builds (where the source is in a cloned git repository).
  * Potential for reducing load on CI.  Although more work may be needed, this kind of filtering combined with the experimental content addressing features of Nix reduce the required rebuilds.
* In the past rather cryptic error messages were given when an attempt was made to use an old version of GHC on a platform Haskell.nix did not support it.  In some cases Haskell.nix would even attempt to build GHC and only fail after some time.  Better error messages are now given right away when an attempt is made to use a GHC version that is not supported for a particular platform [#1411](https://github.com/input-output-hk/haskell.nix/pull/1411)

## Version Updates

* GHC 9.2.2 was added [#1394](https://github.com/input-output-hk/haskell.nix/pull/1394)

## Bug fixes

* `gitMinimal` replaces `git` to reduce the dependency tree of `cabalProject` functions [#1387](https://github.com/input-output-hk/haskell.nix/pull/1387)
* Less used of `allowSubstitutes=false` [#1389](https://github.com/input-output-hk/haskell.nix/pull/1389)
* Fixed `aarch64-linux` builds by using correct boot compiler [#1390](https://github.com/input-output-hk/haskell.nix/pull/1390)
* `icu-i18n` package mapping added to make `text-icu` build [#1395](https://github.com/input-output-hk/haskell.nix/pull/1395)
* Fixes needed for newer `nixpkgs` versions
  * Use list for `configureFlags` [#1396](https://github.com/input-output-hk/haskell.nix/pull/1396)
  * The spdx json file is in a `.json` output [#1397](https://github.com/input-output-hk/haskell.nix/pull/1397)
  * `gdk_pixbuf` is now `gdk-pixbuf` [#1398](https://github.com/input-output-hk/haskell.nix/pull/1398)
* Replaced deprecated NixOS binary cache settings in docs [#1410](https://github.com/input-output-hk/haskell.nix/pull/1410)
* Enable static build of `secp256k1` on musl [#1413](https://github.com/input-output-hk/haskell.nix/pull/1413)

Finally, we’d like to thank all the awesome contributors, who make `haskell.nix` a thriving open source project! :heart:
