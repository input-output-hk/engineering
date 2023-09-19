# Internship in GHC at IOG

My name is Bartłomiej Cieślar, and today marks the end of my 5-month long
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

However, consider the following (nb real) issue: there are several functions in
the module `Data.List` (`lines`, `words`, `unlines` and `unwords`) that have
been moved to `Data.String`; however, their exports cannot be removed from
`base`, since in doing so it would immediately break any code using those
functions through `Data.List` without any heads-up. This is where the new
deprecated warnings come in. Under the new syntax, it is possible to write
deprecation pragmas next to the export definitions:

```haskell
module Mod where
       ( {-# DEPRECATED "Moved to Mod2" #-} foo
       , {-# WARNING "Moved to Types" #-} T(..) )
import Mod2 (foo)
import Types (T(..))
```

Note that if an identifier is exported both with and without a warning, it will
not emit a warning at the importing module. A warning flag
`-Wincomplete-export-warnings` prompts the user about such occurrences

```haskell
module Mod3 where
       ( {-# DEPRECATED "Moved to Mod2" #-} foo
       , foo ) -- foo will not be deprecated
import Mod2 (foo)
```

simmilarly, if an identifier is imported from modules that both deprecate and do
not deprecate it, it will not be warned about (unless explicitly mentioned in
the import list of that module or explicitly qualified with that module)

```haskell
module Mod4 where
import Mod1 (foo) -- warning here since explicitly mentioned
import Mod3

bar = foo -- no warning here since imported without deprecation through Mod3
baz = Mod1.foo -- warning here since the function is qualified
```

The new feature can be used from the GHC version 9.8

## Deprecating Instances

Simmilar to exports, there was previously no way to deprecate single instances
of type classes. For example, the `NFData` class is responsible for implementing
deep strictness on types. However, there is an instance of it for

```haskell
instance (Enumerate a, NFData b) => NFData (a -> b)
```

which does not make much sense and is pretty inefficient. However, it also
cannot be removed since this would suddenly break all code using this feature,
not giving the library users time to update their code. However, now there is a
way to do add deprecation pragmas to instances:

```haskell
instance {-# DEPRECATED "Do not use" #-} (Enumerate a, NFData b) => NFData (a -> b)
```

which will emit a warning every time a type class constraint is solved to this
instance. It is also possible to add warnings to derived instances, although
they have to be derived in explicit statements:

```haskell
derived instance {-# DEPRECATED "Will be removed soon" #-} Show T
```

The new feature can be used from the GHC version 9.10

## Incomplete Record Selectors

In haskell, one can name the fields of a data type constructor, in which case
that constructor is called a record:

```haskell
data T = T1 { x :: Int } | T2 Bool | T3 { x :: Int, y :: Char }
```

In order to access the fields of a record, one can pattern match on it:

```haskell
foo1 :: T -> Int
foo1 (T1 {x = val}) = val
foo1 _              = 0
```

or use a record selector function:

```haskell
foo2 :: T -> Int
foo2 t = x t + 2
```

However, the issue is that a record selector is not defined for the constructors
which do not have that field; in this case, `foo2` would fail on the constructor
`T2`. Such record selectors are called incomplete. There is already a warning
`-Wpartial-fields` that warns about such record fields at the definition site;
however, it is sometimes desirable to use incomplete record selectors because of
some usage pattern of a library (e.g. the field is defined for only one
constructor). Therefore, a new warning `-Wincomplete-record-selectors` is now
available, and warns about such occurences of incomplete record selectors that
could potentially fail

```haskell
foo3 :: T -> Int
foo3 (T1 {x = x}) = x
foo3 t = x t -- warning emitted here

foo4 :: T -> Int
foo4 (T2 _) = 0
foo4 t = x t -- warning not emitted here, since T2 is handled by the previous case
```

this also works with GADTs

```haskell
data G a where
    G1 :: {z :: Int, r :: Char} -> G a
    G2 :: {r :: Char} -> G Int

bar1 :: G a -> Int
bar1 g = r g -- warning emitted here

bar2 :: G Char -> Int
bar2 g = r g -- warning not emitted here, since G2 cannot occur
```

The new feature can be used from the GHC version 9.10

## Other minor contributions

For the purpose of this part, let us assume that module `Mod` has the following definition:

```haskell
module Mod where
data X = X {x :: Int, y :: Char}
{-# DEPRECATED x "Will be removed soon" #-}
```

### Broken deprecations for record fields

Since GHC 9.4, deprecations of record fields were not emitted for record
selectors, the usage of `HasField` or using the overloaded record dot syntax

```haskell
module Mod2 where
import Mod
import GHC.Records
foo :: X -> Int
foo = x -- no warning emitted here

bar1 :: HasField "x" t Int => t -> Int
bar1 t = getField @"x" t

bar2 :: X -> Int
bar2 = bar1 -- no warning emitted here

baz :: X -> Int
baz t = t.x -- no warning emitted here
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
