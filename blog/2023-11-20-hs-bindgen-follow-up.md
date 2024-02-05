---
slug: 2023-11-20-hs-bindgen-follow-up
title: Another step forward, a greater interoperability between Rust and Haskell
authors: [yvan]
tags: [cabal, rust, bindgen]
---

This article is a follow-up of [One step forward, an easier interoperability between Rust and Haskell](https://engineering.iog.io/2023-01-26-hs-bindgen-introduction) article. The previous post was addressed to the Haskell community, while this one will go more in depth of `hs-bindgen` implementation details, provide more complex usage examples and is written with in mind readers among Rust folks.

> N.B. This post will not discuss `cargo-cabal` implementation as it's a dumb simple CLI that end up to be written in Rust (to be used as a `cargo` command) but could otherwise be a simple shell script. But some projects update about it, I started to add [preliminary Haskell Tool Stack support](https://github.com/yvan-sraka/cargo-cabal/pull/5) and testers among Stack users are very welcome.

## General Architecture

`hs-bindgen` is split into several crates, organized as the following dependency graph:

```
hs-bindgen
├── hs-bindgen-attribute
│   ├── hs-bindgen-types
│   ├── reflexive
│   └── warning
└── hs-bindgen-traits
```

Let's recap:

- [`hs-bindgen`](https://github.com/yvan-sraka/hs-bindgen) is the big wrapper around other crates, it's also the root for documentation and test suite ;

- [`hs-bindgen-attribute`](https://github.com/yvan-sraka/hs-bindgen-attribute) defines the attribute procedural macro, it's where live both Rust and Haskell code generation, so it's the core logic/component of `hs-bindgen` ;

- [`hs-bindgen-types`](https://github.com/yvan-sraka/hs-bindgen-types) defines the mapping between Rust ↔ Haskell primitive types, types available are so far the intersection of those defined in Haskell [`Foreign.C.Types`](https://hackage.haskell.org/package/base/docs/Foreign-C-Types.html) and Rust [`core::ffi`](https://doc.rust-lang.org/core/ffi/) ;

- [`reflexive`](https://github.com/yvan-sraka/reflexive) and [`warning`](https://github.com/yvan-sraka/warning) are two ~~hacks~~ utility crates. There were written in the purpose of `hs-bindgen` but could be clearly reused in other projects ;

- [`hs-bindgen-traits`](https://github.com/yvan-sraka/hs-bindgen-traits) define `FromReprRust` and `FromReprC` trait and their implementations for `std` types.

<!-- TODO: should I repeat here the core design principles I tried to follow https://github.com/yvan-sraka/hs-bindgen?tab=readme-ov-file#design? -->

There are also a few example repositories, like:

- the [demo](https://github.com/yvan-sraka/demo/blob/main/test/app/Main.hs) (that use `rustcrypto` crate, rather than `cryptonite` unsafe C-bindings) that was presented internally at IOG ;

- a POC that `hs-bindgen` could be used in [`#![no_std]` settings](https://github.com/yvan-sraka/no_std/blob/master/src/main.rs#L1) ;

- and that a Haskell library [can depend](https://github.com/yvan-sraka/test-0.1.0.0/blob/main/test.cabal#L32) on a one [generated with `hs-bindgen`](https://github.com/yvan-sraka/greetings-0.1.0.0/blob/main/src/lib.rs) ;

- and finally an example of [a project that cross-compile to windows](https://github.com/yvan-sraka/haskell-ffi-cross-compilation-demo) using Nix (and Rust [`greetings`](https://github.com/yvan-sraka/greetings) crate as flake input).

## `FromReprC` and `FromReprRust` traits

> Recall from previous article, our bindinding geneneration (bindgen) would basically expand a function defined like:
>
> ```rust
> #[hs_bindgen(greetings :: CString -> IO ())]
> fn greetings(name: &str) { /* ... */ }
> ```
>
> ... into something of the form:
>
> ```rust
> fn greetings(name: &str) { /* ... */ }
>
> #[no_mangle]
> extern "C" fn __c_greetings(__0: *const core::ffi::c_char) -> () {
>     traits::FromReprC::from(greetings(traits::FromReprRust::from(__0),))
> }
> ```

Let’s just check out the implementation of `FromReprC` and `FromReprRust` traits:

```rust
/// Generate C-FFI cast from a given Rust type.
pub trait FromReprC<T>: private::CFFISafe {
    fn from(_: T) -> Self;
}

/// Generate safe Rust wrapper from a given C-FFI type.
pub trait FromReprRust<T: private::CFFISafe> {
    fn from(_: T) -> Self;
}
```

Both traits define just a straightforward conversion `from` method. Let's take a closer look to `private::CFFISafe` trait bound.

> The trait `CFFISafe` is sealed into `private` module and so cannot be implemented for types outside `hs-bindgen-trait` crate. c.f. https://rust-lang.github.io/api-guidelines/future-proofing.html#c-sealed

What types implement `private::CFFISafe` trait? Types defined in `core::ffi` (meaning `i8`, `i16`, `i32`, `i64`, `u8`, `u16`, `u32`, `u64`, `f32`, `f64` and `()` as a counterpart to C `void`) and `*const` of those types. Those are the exact set of types that have a specified memory layout under the C calling convention (used by `extern` Rust keyword by default).

`hs-bindgen-traits` provide implementations for Rust `std` types, let's look at some examples:

-  a way to turn a foreign C (`*const T`) pointer into a (known at compile time) sized array (`&[T; N]`):

  ```rust
  impl<T, const N: usize> FromReprRust<*const T> for &[T; N]
  where
      *const T: private::CFFISafe,
  {
      #[inline]
      #[allow(clippy::not_unsafe_ptr_arg_deref)]
      fn from(ptr: *const T) -> Self {
          let s = unsafe { std::slice::from_raw_parts(ptr, N) };
          s.try_into().unwrap_or_else(|_| {
              let ty = std::any::type_name::<T>();
              panic!("impossible to convert &[{ty}] into &[{ty}; {N}]");
          })
      }
  }
  ```

-  a way to turn a Rust dynamically sized vector (`Vec<T>`) into a foreign C (`*const T`) pointer:

  ```rust
  impl<T> FromReprC<Vec<T>> for *const T
  where
      *const T: private::CFFISafe,
  {
      #[inline]
      fn from(v: Vec<T>) -> Self {
          let x = v.as_ptr();
          // since the value is passed to Haskell runtime we want Rust to never
          // drop it!
          std::mem::forget(v);
          x
      }
  }
  ```

As you can see, those implementations are a way to hide some unsafe operations and memory management considerations. Dealing with those details with traits abstraction invites the user to define type-based behaviors that could be nicely composed. E.g., here is a generic implementation that will enable the possibility to use any `*mut T` in a user function annotated by attribute macros `#[hs-bindgen]`:

```rust
impl<T> FromReprRust<*const T> for *mut T
where
    *const T: private::FromReprRust,
{
    #[inline]
    fn from(x: *const T) -> Self {
        x as *mut T
    }
}
```

## Handling user-defined types

This part will give advanced examples showing off how to use the framework introduced here to handle arbitrary data-type by just making it implement `FromReprRust` and `FromReprC`.

<!-- TODO: a more complete example, when we now try to pass a custom type to our interface, what about using serde behind the hood to deserialize a C String? -->

## Write macros _à la Rust_

This project could be basically summed up as the writing of a fancy Rust macro. When I started writing `hs-bindgen` I struggled with the difficulty of building a correct mental model around how works (and how to write) Rust macros! Now I basically what to share the insight I got and the hacks I wrote.

First, recall that macros are a static construct/step (basically, they are built by `rustc` and then loaded at compile-time). In other words, it's a way to write a _“meta”_ program that operates on your program. What does that mean in the context of Rust? E.g., that `hs-bingen` could be used in `no_std` environment, while the code of `hs-bindgen` relies a lot on `std` constructs! If that feels counterintuitive, you can think about it the same way that `rustc` sources or written in Rust and relies on `std` too while being able to produce freestanding _“bare”_ binaries.

<!-- TODO: add links to documentation … -->

Rust macros are often sold out as _“hygienic”_: meaning that they can't change things outside a given scope (or to be more accurate: a given span). Why talk about spans rather than scopes? Because macro expansion happens just after lexing (so really early in compilation). It means that e.g., if you wrote a procedural macro (proc-macro), your function will consume a `TokenStream` … Procedural macros are useful every time you want to do something more advanced than regex-based search and replace (what provide [`macro_rules!`](https://doc.rust-lang.org/rust-by-example/macros.html#macro_rules)), and that could opt out hygiene!

> n.b. One should be concerned by the fact that Rust parser can’t be currently just used as an external crate (as Guillaume Gomez https://toot.cat/@imperio/109756865315601567 suggested informally while a Parisian Rust Meetup I attended to present the project). So, currently most proc-macro crates rely on David Tolnay [`syn`](https://github.com/dtolnay/syn) crate …

### The problem I tried to work around …

I would like to know in a project scope if type `X` implement trait `T` while a procedural macro (here an attribute macro) proceeds some Rust program (here the type of function signature). But I can't know that because, as just recall, proc-macros proceed in a stream of tokens (understand a fancy string buffer without any semantic meaning). So, the problem lives in the lack of reflexivity of the Rust language …

<!-- TODO: add links to documentation … -->

> _Reflexivity_ is a concept that I got hard times to properly explain (and understand myself) mostly to folks that don’t get used to Lisp derivates (n.b. some languages are not S-Expression based but share a lot with Scheme semantics, like Erlang or JavaScript, and so offer reflexivity features).
>
> Let’s stick to the mirror metaphor: reflexivity is the ability of a programming language to manipulate its own constructs. Think about the `eval` keyword, which is a really dynamic behavior. And indeed it's way more easy to implement such feature in a VM/JIT-based language rather than in a statically typed compiled language with well separated stages of intermediate representations.

<!-- TODO: introduce `reflexive` crate https://github.com/yvan-sraka/reflexive, explain why I keep the hack behind an optional feature -->

### The problem everyone should be concerned about …

Most Rust ergonomics are deeply living in crates that heavily use proc-macros under the hood … ask yourself how much of your projects dependent upon it? As of the time of writing of this article, I counted that 8 of the top 10 crates provide macros as default interface to the user.

This is bad! For several reasons: First, I will argue that macros are a massive escape hatch in a language that lacks (mainly in its type system) of expressiveness. Second, macros (and mainly procedural macros that depend on `syn` to parse your Rust code) slow down _a lot_ of your whole compilation. Finally, and that's the issue I tried to tackle here, `proc-macros` can't throw warnings in `stable` Rust … because `Diagnostic` API currently only available in `nightly`.

<!-- TODO: introduce `warning` crate https://github.com/yvan-sraka/warning, explain why this work around Diagnostic API -->

## Shipping experiments to production

One should ever ask, what's the polishing that my PoC needs to gain the “production-ready” grade? That's what this paragraph is about, sharing those little happy accidents and last, very last, tweaks I made to this project and why.

First I should mention the excellent [Rust API Guidelines Checklist](https://rust-lang.github.io/api-guidelines/checklist.html), and related but not mention in the checklist, every project should mention their MSRV (Minimum Supported Rust Version), there is even a CLI tool to determine it: [`cargo msrv`](https://github.com/foresterre/cargo-msrv).

Then goes the corner cases handling, I don't need to advocate the writing of test suite. But I can share the story of a some fancy bug I encountered: GHC `darwin_aarch64` implementation of `ccall`/`capi` ABI is broken, the 9th function argument of a C function is mysteriously lost into the void … So, `hs-bindgen` will nicely throw you an error if you try to use too many (more than 8) function arguments.

<!-- TODO: add link to code? https://github.com/yvan-sraka/hs-bindgen-attribute/blob/main/src/rust.rs#L27 -->

I got at FOSDEM the question by David Thrane Christiansen of “Why not an `inline-rust`?” which would have the same ergonomics as [`inline-c`](https://hackage.haskell.org/package/inline-c). I argued I don't think it's a good idea because the Rust toolchain isn't part of the Haskell GHC implementation: It's not like writing inline assembly, and it would be a wrong separation of concern to offer such an API. E.g., how and where did I declare crates dependencies in an `inline-rust` mode?

## Conclusion

I would like to thank really much my long-time pair programming buddy @adrien-zinger that really gave me a lot of insight that unstuck me in the real thing implementation. He basically had the paternity of the traits-based approach.

The next step would be to reuse the same framework to propose a clean and modern `c-bindgen` implementation (the current one suffers from [some design issues](https://github.com/eqrion/cbindgen/blob/master/docs.md#writing-your-c-api)). Help is really welcomed, and I would be happy to mentor anyone wanting to dirty their hands, learning some _advanced_ Rust as side effect!

<!-- TODO: maybe say a word about:
* that I now allow binding to not return `IO` type 
* that I introduced  a `depreciation` attribute, generating https://downloads.haskell.org/~ghc/9.0.1/docs/html/users_guide/exts/pragmas.html#warning-deprecated-pragma rather than directly using https://doc.rust-lang.org/reference/attributes/diagnostics.html#the-deprecated-attribute.
* that I mentored contributions in impl Room at next EuroRust and added project to https://github.com/RustBeginners/awesome-rust-mentors/edit/master/_includes/projects.md -->