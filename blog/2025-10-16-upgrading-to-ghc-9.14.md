---
slug: upgrading-to-ghc-9.14
title: "Upgrading to ghc-9.14"
authors: [erikd]
tags: [ghc-9.14]
---



<!-- truncate -->

At IOG we have several large and complex Haskell code bases. We also
like to make sure our various code bases compile with new versions of
GHC when those new versions come out. The alternative would be to just
use one supported compiler and to update to a new supported compiler
every couple of years. We feel that this is sub optimal for a number
of reasons:

* Fixing breakages in our code due to the new compiler, after skipping
many releases, is more difficult than keeping up-to-date with the
current compiler.

* Keeping up with the latest compiler allows us to test for performance
regressions as soon as new compiler versions are released. An example
where we didn't do this we found performance regression in `ghc-9.6`
which had been
[introduced in `ghc-9.2`](https://engineering.iog.io/2024-10-17-ghc-update/).

* Contributions that we at IOG make to GHC generally only end up in the
latest release.

* If we have to make material changes to our code base for this compatibility
change; this also means that we can not rely on existing performance, and
similar metrics we know about the code, and will have to re-run them for
whatever compiler we are currently using.

We will now look at the issues of updating one of our code bases to
build with `ghc-9.14` (currently a pre-release). This code base already
compiles without any issue with `ghc-9.12.2`.

The code base is
[cardano-ledger](https://github.com/IntersectMBO/cardano-ledger/)
which is a single Git repository containing 28 Haskell/Cabal packages,
1130 files and a little over 200,000 lines of Haskell code. This code
base also uses many relatively new Haskell features, like type families,
pattern synonyms, data kinds, poly kinds and so on.

We are currently only using a pre-release `ghc-9.14.0.20251007` and
the diff to the code relative to code that compiles with `ghc-9.12.2`
has the following statistics:

```
> git diff master | diffstat
 ...
  127 files changed, 1440 insertions(+), 98 deletions(-)
```

That means that over 10% of all files in this project needed
modifications. The modifications required were exclusively of two
types:

### The requirement to use `data` instead of `pattern` in import/export lists.
This change is explained in the
[changelog](https://downloads.haskell.org/ghc/9.14.1-alpha1/docs/users_guide/exts/explicit_namespaces.html#the-data-keyword-in-import-export-lists)
for `ghc-9.14`. Since we need to support older compilers as well as
`ghc-9.14`, we need special `CPP` handling of the form:
```
+#if __GLASGOW_HASKELL__ >= 914
+  data RequireTimeExpire,
+#else
   pattern RequireTimeExpire,
+#endif
```

### Changes on warnings on constraints
  The first pre-release of `ghc-9.14` had a bug,
[GHC26381](https://gitlab.haskell.org/ghc/ghc/-/issues/26381)
  Even after this bug is fixed, there is still some significant 
  changes around warning about redundant constraints. This one was
  annoying because some constraints are **required** for earlier 
  compilers and result in redundant constraint warning for `ghc-9.14`.
  The solution was:
```
+#if __GLASGOW_HASKELL__ < 914
+  -- This constraint is REQUIRED for ghc < 9.14 but REDUNDANT for ghc >= 9.14
+  -- See https://gitlab.haskell.org/ghc/ghc/-/issues/26381#note_637863
-  , Show (ContextError era)
+#endif
```

## Possible Solutions

There are a number of potential solution to these problems but none are
considered satisfactory.

### Turn off `-Werror`

We find this possible solution unsatisfactory, because we do not want to
ignore warnings and without `-Werror` warnings can just scroll past at a
fast pace and can easily be missed.

### Turn off `-Wredundant-constraints`

We find this solution unsatisfactory because at around the time this
warning first we had some code that was often difficult to modify and
maintain because it was overly constrained by constraints that were
actually not needed.

## Conclusion

Where there are two possible solutions, neither are considered better
than the `CPP` hack alternative. We at IOG hope that the GHC developers
become more aware of how compiler changes impact users, particularly
those with large complex code bases. In the meantime we are experimenting
with ideas in https://www.stable-haskell.org, which we hope will eventually allow
us to make this easier for us, and provide valuable changes to be we can upstream.
