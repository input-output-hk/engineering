---
slug: 2023-09-22-internship
title: "Internship in GHC at IOG"
authors: [bartek]
tags: [ghc,internship]
---

# Internship in GHC at IOG

My name is Bartłomiej Cieślar, and I have done a 5-month long
internship at IOG. Throughout the internship my work was focused on improving
the Glasgow Haskell Compiler. In this post I share a bit about the work that I
did and the new features that will be available in GHC 9.10 thanks to me.

## Deprecating Exports

Originally in GHC, the only way to deprecate usages of certain things was to
deprecate their definitions, as in the following example:

```haskell
foo :: Int -> Int
foo x = x * x

{-# DEPRECATED foo "Do not use" #-}
```

However, consider the following (real) issue: there are several functions in
the module `Data.List` (`lines`, `words`, `unlines` and `unwords`) that have
been moved to `Data.String`; however, their exports cannot be removed from
`base`, since in doing so it would immediately break any code using those
functions through `Data.List` without any heads-up. This is where the new
deprecated warnings come in.

Under the new syntax (available from GHC 9.8), it is possible to write deprecation and warning annotations next to the export definitions:

```haskell
module Data.List where
       ( {-# DEPRECATED "Moved to Data.String" #-} lines
       , {-# WARNING "Will be removed in the next release!" #-} SomeType(..) )
import Data.String (lines)
import Types (SomeType(..))
```

There are two corner cases:
- an identifier imported from several modules leading to inconsistent annotations in the importing module
- an identifier exported more than once with inconsistent annotations in the exporting module

The first case is illustrated with the following example:

```haskell
module Good where
foo :: Int
foo = 10
----------------------
module Wrong
  ( {-# DEPRECATED "Moved to Good" #-} foo
  )
where
import Good
----------------------
module Test where

import Wrong (foo) -- warning here since explicitly mentioned in Wrong's import list
import Good

bar = foo       -- no warning here since imported without deprecation from Good
baz = Wrong.foo -- warning here since the import is qualified with Wrong
```

`foo` is imported from both `Good` and `Wrong` modules but it is deprecated only in `Wrong`'s export list.
Warnings are emitted every time the identifier is explicitly used from `Wrong`:
- in `Wrong`'s import list
- when an occurrence is qualified with `Wrong`

```
> ghc-9.8 Test.hs
[1 of 3] Compiling Good             ( Good.hs, Good.o )
[2 of 3] Compiling Wrong            ( Wrong.hs, Wrong.o )
[3 of 3] Compiling Test             ( Test.hs, Test.o )

Test.hs:3:15: warning: [GHC-68441] [-Wdeprecations]
    In the use of ‘foo’ (imported from Wrong):
    Deprecated: "Moved to Good"
  |
3 | import Wrong (foo)
  |               ^^^

Test.hs:7:7: warning: [GHC-68441] [-Wdeprecations]
    In the use of ‘foo’ (imported from Wrong):
    Deprecated: "Moved to Good"
  |
7 | baz = Wrong.foo
  | 
```

The second case is illustrated with the following example:

```haskell
module Both
  ( {-# DEPRECATED "Moved to Good" #-} foo
  , foo
  )
where
import Good (foo)
```

`foo` identifier is exported twice with inconsistent annotions in module `Both`: both with and without a deprecation annotation.
A module importing `foo` from `Both` won't raise a deprecation warning (there is a non-deprecated export after all).
To help avoiding this situation, a new compiler flag `-Wincomplete-export-warnings` can be enabled (it is included in `-Wall`)
to make the compiler warn about such inconsistent annotations in the defining module:

```
> ghc-9.8 Both.hs -Wall
[1 of 2] Compiling Good             ( Good.hs, Good.o )
[2 of 2] Compiling Both             ( Both.hs, Both.o )

Both.hs:2:5: warning: [GHC-94721] [-Wincomplete-export-warnings]
    ‘foo’ will not have its export warned about
    missing export warning at Both.hs:3:5-7
  |
2 |   ( {-# DEPRECATED "Moved to R" #-} foo
  |     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Both.hs:3:5: warning: [GHC-47854] [-Wduplicate-exports]
    ‘foo’ is exported by ‘foo’ and ‘{-# DEPRECATED "Moved to R" #-}
                                    foo’
  |
3 |   , foo
  |     ^^^
```

References:
- [GHC proposal #134](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0134-deprecating-exports-proposal.rst)
- [GHC issue #4879](https://gitlab.haskell.org/ghc/ghc/-/issues/4879)
- [GHC merge request !10283](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/10283)


Note: The WARNING pragma behaves exactly like the DEPRECATED pragma.
In previous examples and in the rest of this post we always use the
DEPRECATED pragma but the WARNING pragma could have been used too.

## Deprecating Instances

Similar to exports, there was previously no way to deprecate single instances
of type classes. For example, the `NFData` class is responsible for implementing
deep strictness on types and there is an instance of it for functions
which does not make much sense and is pretty inefficient:

```haskell
instance (Enumerate a, NFData b) => NFData (a -> b)
```

However, it cannot be removed since this would suddenly break all code using this instance,
not giving the library users time to update their code.

From GHC 9.10, there will be a way to do add deprecation pragmas to instances:

```haskell
instance {-# DEPRECATED "Do not use" #-} (Enumerate a, NFData b) => NFData (a -> b)
```

which will emit a warning every time a type class constraint is solved to this
instance. It is also possible to add warnings to derived instances, although
they have to be derived with standalone `deriving` declarations:

```haskell
deriving instance {-# DEPRECATED "Will be removed soon" #-} Show T
```

References:
- [GHC proposal #575](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0575-deprecated-instances.rst)
- [GHC issue #17485](https://gitlab.haskell.org/ghc/ghc/-/issues/17485)
- [GHC merge request !10902](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/10902)

## Incomplete Record Selectors

In haskell, one can name the fields of a data type constructor, in which case
that constructor is called a record:

```haskell
data T = T1 { fieldX :: Int } | T2 Bool | T3 { fieldX :: Int, fieldY :: Char }
```

In order to access the fields of a record, one can pattern match on it:

```haskell
foo1 :: T -> Int
foo1 (T1 {fieldX = val}) = val
foo1 _                   = 0
```

or use a record selector function:

```haskell
foo2 :: T -> Int
foo2 t = fieldX t + 2
```

The issue is that a record selector is not defined for the constructors
which do not have that field. Such record selectors are called incomplete.
In our example, `fieldX` and `fieldY` are incomplete record selectors.
As a consequence, calling `foo2` on a value constructed with constructor `T2`
would fail at runtime with an exception becaues `T2` doesn't have a `fieldX` field. 

There is already a warning `-Wpartial-fields` that warns about such record fields at the **definition site**:

```
> ghc T.hs -Wpartial-fields
[1 of 1] Compiling T                ( T.hs, T.o )

T.hs:3:15: warning: [GHC-82712] [-Wpartial-fields]
    Use of partial record field selector: ‘fieldX’
  |
3 | data T = T1 { fieldX :: Int } | T2 Bool | T3 { fieldX :: Int, fieldY :: Char }
  |               ^^^^^^

T.hs:3:63: warning: [GHC-82712] [-Wpartial-fields]
    Use of partial record field selector: ‘fieldY’
  |
3 | data T = T1 { fieldX :: Int } | T2 Bool | T3 { fieldX :: Int, fieldY :: Char }
  |                                                               ^^^^^^
```

Note: the message `Use of partial record field selector: ...` should better be reworded as `Definition of partial record field selector: ...`.

However:
1. it is only a warning because incomplete record selectors are sometimes desirable.
2. it is only visible when compiling a module defining partial record field selectors, not when a partial record field selector field is used in some client module.

Therefore, I've implemented a new `-Wincomplete-record-selectors` warning (available from GHC 9.10)
that warns about **occurrences** of incomplete record selectors that **can't be proved not to fail**.


```haskell
foo3 :: T -> Int
foo3 (T1 {fieldX = x}) = x
foo3 t                 = fieldX t -- warning emitted here

foo4 :: T -> Int
foo4 (T2 _) = 0
foo4 t      = fieldX t -- warning not emitted here, since T2 is handled by the previous equation
```

```
> ghc-9.10 Test.hs -Wincomplete-record-selectors
Test.hs:7:26: warning: [GHC-17335] [-Wincomplete-record-selectors]
    The application of the record field ‘fieldX’ may fail for the following constructors: T2
  |
7 | foo3 t                 = fieldX t -- warning emitted here
  |  
```

This also works with GADTs for which we need to take types into account to know which constructors
are allowed to occur. In the following example, it doesn't make sense to warn in `bar2` about handling
the case where `g` is constructed with `G2` because `g`'s type prevents it.

```haskell
data G a where
    G1 :: {fieldZ :: Int, fieldR :: Char} -> G a
    G2 :: {fieldR :: Char} -> G Int

bar1 :: G a -> Int
bar1 g = fieldZ g -- warning emitted here

bar2 :: G Char -> Int
bar2 g = fieldZ g -- warning not emitted here, since G2 cannot occur
```

References:
- [GHC proposal #516](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0516-incomplete-record-selectors.rst)
- [GHC issue #18650](https://gitlab.haskell.org/ghc/ghc/-/issues/18650)
- [GHC merge request !10736](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/10736)

## Other minor contributions

For the purpose of this part, let us assume that module `Mod` has the following definition:

```haskell
module Mod where
data X = X {x :: Int, y :: Char}
{-# DEPRECATED x "Will be removed soon" #-}
```

### Broken deprecations for record fields

Since GHC 9.4, deprecation warnings attached to record fields were not emitted when record fields
were accessed via record selectors, the usage of `HasField`, or with the overloaded record dot syntax:

```haskell
module Mod2 where
import Mod
import GHC.Records
foo :: X -> Int
foo = x -- no warning emitted here (record selector)

bar1 :: HasField "x" t Int => t -> Int
bar1 t = getField @"x" t

bar2 :: X -> Int
bar2 = bar1 -- no warning emitted here (HasField constraint)

baz :: X -> Int
baz t = t.x -- no warning emitted here (record dot syntax)
```

I can happily say that with the help of Sam Derbyshire and Simon Peyton-Jones
the bug was fixed and this feature will work again fully in GHC 9.8.

References:
- [GHC issue #23279](https://gitlab.haskell.org/ghc/ghc/-/issues/23279)
- [GHC merge request !10991](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/10991)

### Custom deprecations in record wild card syntax

In GHC, one can pattern match on all fields of a record using the record wild
card syntax, which will assign the value of the all remaining record fields to
the variables with the same name:

```haskell
import Mod
foo :: X -> Char
foo (X {..}) = y
```

however, when using this syntax, custom deprecations of record fields won't be
emitted even if those fields are deprecated:

```haskell
bar :: X -> Int
bar (X {..}) = x -- no warning
```

this was because if there were a lot of fields deprecated in the pattern match,
the warnings would have been noisy. However, since GHC 9.10 it will be changed
so that the custom warnings for record fields will be emitted only if the
variables to which their values are assinged to are used in the body of the
function.

References:
- [GHC issue #23382](https://gitlab.haskell.org/ghc/ghc/-/issues/23382)
- [GHC merge request !11043](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/11043)
