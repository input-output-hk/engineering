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
deprecate their definitions

```haskell
foo :: Int -> Int
foo x = x * x

{-# DEPRECATED foo "Do not use" #-}
-- Note: The DEPRECATED pragma can always be substituted with a WARNING pragma
```

However, consider the following (real) issue: there are several functions in
the module `Data.List` (`lines`, `words`, `unlines` and `unwords`) that have
been moved to `Data.String`; however, their exports cannot be removed from
`base`, since in doing so it would immediately break any code using those
functions through `Data.List` without any heads-up. This is where the new
deprecated warnings come in.

Under the new syntax (available from GHC 9.8), it is possible to write deprecation pragmas next to the export definitions:

```haskell
module Mod where
       ( {-# DEPRECATED "Moved to Mod2" #-} foo
       , {-# WARNING "Moved to Types" #-} T(..) )
import Mod2 (foo)
import Types (T(..))
```

There are two corner cases:
- an identifier exported more than once with inconsistent annotations
- an identifier imported from several modules leading to inconsistent annotations in the importing module

In the following example, `foo` identifier is exported twice with inconsistent annotions:

```haskell
module M where
       ( {-# DEPRECATED "Moved to R" #-} foo
       , foo
       )
import R (foo)
```

As `foo` is exported from `M` both with and without a deprecation annotation, a module importing `foo` from `M` won't raise a deprecation warning.
A new compiler flag `-Wincomplete-export-warnings` can be enabled to ake the compiler warn about such inconsistent annotations in the defining module.

The second case is illustrated with the following example: `foo` is imported from both `Good` and `Bad` modules but it is
deprecated only in `Bad`'s export list.

```haskell
import Bad (foo) -- warning here since explicitly mentioned in Bad's import list
import Good

bar = foo     -- no warning here since imported without deprecation from Good
baz = Bad.foo -- warning here since the import is qualified with Bad
```

Warnings will be emitted every time the identifier is explicitly used from `Bad`:
- in `Bad`'s import list
- when an occurrence is qualified with `Bad`


## Deprecating Instances

Similar to exports, there was previously no way to deprecate single instances
of type classes. For example, the `NFData` class is responsible for implementing
deep strictness on types. However, there is an instance of it for

```haskell
instance (Enumerate a, NFData b) => NFData (a -> b)
```

which does not make much sense and is pretty inefficient. However, it also
cannot be removed since this would suddenly break all code using this feature,
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

There is already a warning `-Wpartial-fields` that warns about such record fields at the **definition site**.
However, it is only a warning because incomplete record selectors are sometimes desirable.

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
the bug was fixed and this feature will work again fully in GHC 9.10

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
