---
slug: 2022-04-28-on-the-inlining-of-integer-and-natural-operations
title: On the inlining of Integer and Natural operations
authors: []
tags: [ghc]
custom_edit_url: null
---
**Sylvain Henry (sylvain.henry@iohk.io)** 

In this post I discuss the inlining of Integer and Natural operations in Haskell. It’s a promising performance work I’ve been conducting six months ago, which was blocked by an independent issue, but that I will likely resume soon as the issue has been fixed in the meantime.
---

To follow this post, you must know that `Natural` numbers are represented as follows in `ghc-bignum`:

```haskell
-- | Natural number
--
-- Invariant: numbers <= WORD_MAXBOUND use the `NS` constructor
data Natural
   = NS !Word#
   | NB !BigNat#
```

Small naturals are represented with a `Word#` and large ones with a `BigNat#` (a `ByteArray#`).

Now consider the following simple example using Natural:

```haskell
-- | Add 2 to a Word. Use Natural to avoid Word overflow
foo :: Word -> Natural
foo x = fromIntegral x + 2
```

There are only small naturals involved: `fromIntegral x` is small because `x` is a `Word`, and `2` is small. We could hope that GHC would use `Word#` primops to implement this and would allocate a `Natural` heap object for the result *only*. However it’s not what happens currently, even in GHC HEAD. In the following STG dump, we can see that a `Natural` heap object is allocated for `x` before calling `naturalAdd` (`let` bindings in STG reflect heap allocations):

```haskell
foo1 = NS! [2##];

foo =
    \r [x_sXn]
        case x_sXn of {
        W# x#_sXp ->
        let { sat_sXq = NS! [x#_sXp]; } in  naturalAdd sat_sXq foo1;
        };
```

Let’s look at `naturalAdd`:

```haskell
-- | Add two naturals
naturalAdd :: Natural -> Natural -> Natural
{-# NOINLINE naturalAdd #-}
naturalAdd (NS x) (NB y) = NB (bigNatAddWord# y x)
naturalAdd (NB x) (NS y) = NB (bigNatAddWord# x y)
naturalAdd (NB x) (NB y) = NB (bigNatAdd x y)
naturalAdd (NS x) (NS y) =
   case addWordC# x y of
      (# l,0# #) -> NS l
      (# l,c  #) -> NB (bigNatFromWord2# (int2Word# c) l)
```

We are clearly in the last case where both arguments are small. It seems beneficial to allow this function to be inlined. If we did we would get:

```javascript
foo =
    \r [x_s158]
        case x_s158 of {
        W# x#_s15a ->
        case addWordC# [x#_s15a 2##] of {
        (#,#) l_s15c ds_s15d ->
        case ds_s15d<TagProper> of ds1_s15e {
          __DEFAULT ->
              case int2Word# [ds1_s15e] of sat_s15f {
              __DEFAULT ->
              case bigNatFromWord2# sat_s15f l_s15c of ds2_s15g {
              __DEFAULT -> NB [ds2_s15g];
              };
              };
          0# -> NS [l_s15c];
        };
        };
        };
```

which produces much better assembly code, especially if there is no carry:

```
    addq $2,%rax       ; add 2 to a machine word
	setc %bl           ; test the carry.
	movzbl %bl,%ebx    ; it could be done
	testq %rbx,%rbx    ; more efficiently
	jne _blk_c17c      ; with "jc"
_blk_c17i:
	movq $NS_con_info,-8(%r12) ; alloc NS datacon value
	movq %rax,(%r12)           ; with the addition result as payload.
	leaq -7(%r12),%rbx         ; make it the first argument
	addq $8,%rbp               ; and then
	jmp *(%rbp)                ; call continuation
...
```

So why aren’t we always inlining `naturalAdd`? We even explicitly disallow it with a `NOINLINE` pragma. The reason is that `naturalAdd` and friends are involved in constant-folding rules.

For example, consider:

```haskell
bar :: Natural -> Natural
bar x = x + 2

baz = bar 0x12345678913245678912345679123456798
```

Currently we get the following Core:

```haskell
bar1 = NS 2##

bar = \ x_aHU -> naturalAdd x_aHU bar1

baz = NB 99114423092485377935703335253042771879834
```

You can see that `baz`  is a constant thanks to constant-folding.

However if we let `naturalAdd` inline we get:

```haskell
baz
  = case bigNatAddWord# 99114423092485377935703335253042771879832 2##
    of ds_d11H
    { __DEFAULT ->
    NB ds_d11H
    }
```

`baz` is no longer a constant.

A solution would be to add constant-folding rules for `BigNat#` functions, such as `bigNatAddWord#`. This is exactly what we have started doing in [#20361](https://gitlab.haskell.org/ghc/ghc/-/issues/20361). Our new plan is:

* Make `BigNat#` operation `NOINLINE` and add constant-folding rules for them
* Make Integer/Natural operations `INLINEABLE` (expose their unfolding)
* Hence rely on constant-folding for `Word#/Int#/BigNat#` to provide constant folding for `Integer` and `Natural`

The good consequences of this plan are:

* Less allocations when bignum operations are inlined and some of the arguments are known to be small/big or fully known (constant).
* `Integer` and `Natural` are less magical: you can implement your own similar types and expect the same performance without having to add new rewrite rules

There were some unforeseen difficulties with this plan though:


1. Some of the rewrite rules we need involve unboxed values such as `BigNat#` and `Word#` and the weren’t supported. Luckily, this has been recently fixed ([#19313](https://gitlab.haskell.org/ghc/ghc/-/issues/19313)) by removing the “app invariant” ([#20554](https://gitlab.haskell.org/ghc/ghc/-/issues/20554)). Thanks Joachim! That’s the reason why we could resume this work now.
2. Some unfoldings (RHSs) become bigger due to the inlining of bignum operations. Hence they may not themselves be inlined further due to inlining thresholds even if it would be beneficial. A better inlining heuristic would fix this (see [#20516](https://gitlab.haskell.org/ghc/ghc/-/issues/20516)). It will likely be the topic of the next post.
