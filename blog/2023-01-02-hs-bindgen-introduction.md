---
slug: 2023-01-23-hs-bindgen-introduction
title: One step forward, an easier interoperability between Rust and Haskell
authors: [yvan]
tags: [cabal, rust, bindgen]
---

**TL;DR:** This blog post intends to sum up the why and how of [`cargo-cabal`](https://github.com/yvan-sraka/cargo-cabal) and [`hs-bindgen`](https://github.com/yvan-sraka/hs-bindgen), if you’re looking for usage walkthroughs and code examples, check out project READMEs on GitHub!

> **N.B.** quoted paragraphs in this article, give straightforward motivation regarding some systems programming basic concepts, feel free to skip them if you know you’re likely to be already comfortable with ;)

## Context

At IOG we maintain large [Haskell](https://www.haskell.org) codebases and we would like to interface them with some libraries written in [Rust](https://www.rust-lang.org).

Rust is a systems programming language known for its strong static typing guarantees, which make it similar to Haskell (even if a bit less expressive). However, unlike Haskell, Rust does not have a GC (Garbage Collector) and use a compile-time memory management strategy. This mechanism is encoded through its type system with the concept of _“ownership”_ and _“lifetime”_ of values, which can complicate the writing of programs but unlock smaller runtime costs footprint. Rust is becoming increasingly popular in the systems programming and embedded systems domains, and is also used in areas such as cryptography, where performance is critical.

One typical use case concerns cryptographic primitives which must be very performant. The first use case that Iñigo Querejeta Azurmendi (Cardano Lead Cryptography Engineer) brought me consisted in replacing a cryptographic library used by [`cardano-base`](https://github.com/input-output-hk/cardano-base). Namely, replacing [`cryptonite`](https://github.com/input-output-hk/cardano-crypto/blob/develop/cbits/cryptonite_sha512.h), a library written in Haskell and C, with [`sha3`](https://crates.io/crates/sha3), a Rust library (or "crate").

> **Why FFI (Foreign Function Interface)?**
>
> Solving the [interoperability](https://docs.rust-embedded.org/book/interoperability/) problem means:
> 1. designing a protocol that allows two codes written with different languages and using different runtime systems to communicate
> 2. designing tools and methods to build, to bundle, and to distribute such polyglot code bases (what developers fear most)
>
> Why not just use, e.g., [Google Protobuf](https://developers.google.com/protocol-buffers) over a [Unix Domain Socket](https://en.wikipedia.org/wiki/Unix_domain_socket)? How to make a choice in this landscape of solutions available? First, I will argue that we want something fast, say, by reducing its overhead to the minimal footprint of extra computations. So, we want to avoid use of any OS capabilities, like I/Os (that will cost you a full roundtrip) and the needs (De)Serialization.
>
> FFI looks like the right choice, it doesn’t rely on any syscall, at runtime they just behave as a jump in memory. Unfortunately, it comes at a cost of careful special attention on following call-conventions and handling memory management, we will come back to it later!
>
> **To go further:** you can learn more about how to use FFI in Rust by reading the [_The Rustonomicon_](https://doc.rust-lang.org/nomicon/ffi.html) (Unsafe Rust guide) dedicated section, or the dedicated [_Rust FFI Omnibus_](http://jakegoulding.com/rust-ffi-omnibus/) tutorial. The ANSSI (French government security agency) also write about it in [_Secure Rust Guidelines_](https://anssi-fr.github.io/rust-guide/07_ffi.html) guide, and _Rust Embedded_ book have an [Interoperability with C](https://docs.rust-embedded.org/book/interoperability/rust-with-c.html) chapter. On the Haskell side, you way want to take a look at [GHC wiki](https://wiki.haskell.org/GHC/Using_the_FFI) or read the dedicated [_Real-World Haskell_](https://book.realworldhaskell.org/read/interfacing-with-c-the-ffi.html) chapter!

FFI is a feature that's already offered by both `rustc` (Rust compiler) and `ghc` (Haskell compiler). It allows calling a Rust function from Haskell code (and vice versa). Both programming languages define an `extern` keyword that allow users to declare a function symbol that will only be resolved at [linking](https://doc.rust-lang.org/reference/linkage.html) step. N.B. mangling of the function should also be disabled, in Rust it requires decorating functions with [`#[no_mangle]`](https://doc.rust-lang.org/reference/abi.html#the-no_mangle-attribute) attribute.

So, what's lacking in Haskell ecosystem? Let's take a look at what kind of integration other languages offer with Rust:

* From **X** to Rust with X=**C**, [`rust-bindgen`](https://github.com/rust-lang/rust-bindgen)

* From Rust to **X** with X=**C**, [`c-bindgen`](https://github.com/eqrion/cbindgen) and X=**ECMAScript**, [`wasm-bindgen`](https://github.com/rustwasm/wasm-bindgen)

* Both with X=**C++**, [`cxx`](https://github.com/dtolnay/cxx) and X=**Python**, [`PyO3`](https://github.com/PyO3/pyo3)

This list isn't exhaustive but give you a hint, all these projects are about generating bindings (bindgen)!

> **Why bindgen (bindings code generation)?**
>
> Let's sum it up by: _"A good FFI is an FFI that you don't write …"_
>
> FFI are like a blind spot in your type system. Writing them manually is both frankly painful and really dangerous, as your compiler will not save you of non-matching interfaces.
>
> Binding generation comes to the rescue by considerably reducing the room for human errors, and as a bonus, it simplifies a lot of maintainer job with a more readable reduced codebase.

## Example

So, let's look at a minimal example. In this case, if we annotate a function like this:

```rust
use hs_bindgen::*;

/// Haskell type signature are auto-magically inferred from Rust function
/// type! This feature could slow down compilation, and be enabled with:
/// `hs-bindgen = { ..., features = [ "full" ] }`
#[hs_bindgen]
fn greetings(name: &str) {
    println!("Hello, {name}!");
}
```

… it will be expanded to (you can try yourself with [`cargo expand`](https://github.com/dtolnay/cargo-expand)):

```rust
use hs_bindgen::*;

fn greetings(name: &str) {
    println!("Hello, {name}!");
}

#[no_mangle] // Mangling randomize symbols
extern "C" fn __c_greetings(__0: *const core::ffi::c_char) -> () {
    // `traits` module is `hs-bindgen::hs-bindgen-traits`
    // n.b. do not forget to import it, e.g., with `use hs-bindgen::*`
    traits::ReprC::from(greetings(traits::ReprRust::from(__0),))
}
```

… and will also generate the following Haskell code:

```haskell
-- This file was generated by `hs-bindgen` crate and contain C FFI bindings
-- wrappers for every Rust function annotated with `#[hs_bindgen]`

{-# LANGUAGE ForeignFunctionInterface #-}

-- Why not rather using `{-# LANGUAGE CApiFFI #-}` language extension?
--
-- * Because it's GHC specific and not part of the Haskell standard:
--   https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/ffi.html ;
--
-- * Because the capabilities it gave (by rather works on top of symbols of a C
--   header file) can't work in our case. Maybe we want a future with an
--   {-# LANGUAGE RustApiFFI #-} language extension that would enable us to
--   work on top of a `.rs` source file (or a `.rlib`, but this is unlikely as
--   this format has purposely no public specifications).

{-# OPTIONS_GHC -Wno-unused-imports #-}

module Greetings (greetings) where

import Data.Int
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr

foreign import ccall unsafe "__c_greetings" greetings :: CString -> IO (())
```

In Rust, [`extern`](https://doc.rust-lang.org/book/ch19-01-unsafe-rust.html#using-extern-functions-to-call-external-code) is an alias to `extern "C"` that stands for “use the C call convention” rather than `extern "Rust"` that use the Rust one, which is the default implicitly used.

> **Why C ABI (Application Binary Interface)?**
>
> The Rust [ABI](https://doc.rust-lang.org/reference/items/external-blocks.html#abi) (call-convention and types [memory layout](https://cheats.rs/#memory-layout)) isn’t stable. That means that it’s specified internally but could be broken by any `rustc` minor release, building a software on top of it is by definition a “hack” …
>
> And, if we think it’s worth it, we would have to perform our bindgen against a given `rustc` version (and that would be really laborious to maintain) …
>
> So, do not fear the C ABI … because, at least, it is stable!
>
> [1] https://viruta.org/rust-stable-abi.html

## Implementation

The previous code example highlighted that we rely on two constructs: an attribute procedural macro [`#[hs_bindgen]`](https://github.com/yvan-sraka/hs-bindgen), and (internally) `ReprRust` and `ReprC` traits.

> **Why use a Rust macro?**
>
> Binding code generation could have been achieved using an external tool, e.g., `c-bindgen` parses Rust code (before macro expansion) and deduce the right C function signatures.
>
> But rather we choose to define a custom macro (like `cxx`, `wasm-bindgen` and `PyO3` does), and so we require the user to depend on a custom crate. Why?
>
> We want generated bindings to always match the source code used for generation, so we want to force the user to integrate binding generation in the build process.
>
> The natural way to do that in Rust, and to have directly access to source code's AST (Abstract Syntax Tree), rather than trying to implement a parser, is to use a macro!

Wrapping user types by these traits have several benefits:

* Unsupported types are nicely reported as _“the trait `ReprRust<T>` is not implemented for `U`”_ error (that suggest other types that the trait implement to the user);

* The user can extensively always implement these traits for arbitrary types ;

* Provided traits implementation for `std` types take care of memory management [2] ;

* Traits improve a lot of ergonomics by implicitly and safely casting a given type to an FFI-safe one.

> **What's an FFI-safe type?**
>
> `rustc` will complain if a function prefixed by `extern` keyword use as arguments types that are not FFI-safe. FFI-safe types guarantee that a type has a [specified layout](https://doc.rust-lang.org/reference/type-layout.html) (memory representation) by e.g. having a [`#[repr(C)]`](https://doc.rust-lang.org/nomicon/other-reprs.html#reprc) compiler attribute, for the given C call convention.

[2] Speaking of memory management, the strategy implemented here is: freeing the value is the role of the receiver (which has “ownership” of it). This means that, e.g. the type that return a Rust function, will not be [`Drop`](https://doc.rust-lang.org/std/ops/trait.Drop.html) by Rust but rather should be [`free`](https://hackage.haskell.org/package/base/docs/Foreign-Marshal-Alloc.html) on Haskell side!

## DevX

[`cargo-cabal`](https://github.com/yvan-sraka/cargo-cabal) is a CLI tool that helps you, in one simple command, turning a Rust crate into a Haskell Cabal library!

I was heavily inspired by the developer experience that offers [`wasm-pack`](https://rustwasm.github.io/docs/wasm-pack/) or [`maturin`](https://github.com/PyO3/maturin): launched in any Rust project folder, it helps the user to interactively tweak their `Cargo.toml` to make it successfully build a package compatible with NPM / PyPI and even handle the publishing of it.

What `cargo-cabal` actually does is:

* Ask user to add `crate-type = ["staticlib"]` (or `"cdylib"`, dynamic libraries require an extra `build.rs` that is generated by `cargo-cabal`) to their `Cargo.toml` ;

* Generate a custom `X.cabal` linking `rustc` output as `extra-librairies`, and either a ([`naersk`](https://github.com/nix-community/naersk) and [`haskell.nix`](https://github.com/input-output-hk/haskell.nix) based) `flake.nix` or a `Setup.lhs` build customization (to work around this [issue](https://github.com/haskell/cabal/issues/2641)).

## Limitations

What would be the reason to NOT use `cargo-cabal` and `hs-bindgen`? I should mention that `{-# LANGUAGE CApiFFI #-}` language extonsion features, like using C values that are `#define` in a header or `vargs`, are not supported (because they're indeed C specific). E.g. variadic arguments didn't exist in Rust and are usually implemented with [the Builder pattern](https://doc.rust-lang.org/1.0.0/style/ownership/builders.html). Again I want to make obvious that if you're looking for `CApiFFI` features you should embed Rust code in a C library, with e.g. `c-bindgen`, and so not use the tools introduced here!

## What's next?

[`cargo-cabal`](https://github.com/yvan-sraka/cargo-cabal) and [`hs-bindgen`](https://github.com/yvan-sraka/hs-bindgen) combined are less than 1000 LoC, they also support Rust `#[no_std]` code, and I would be glad to keep them as [KISS](https://en.wikipedia.org/wiki/KISS_principle) and modular as possible. But there is still room for improvements, e.g. by adding traits' implementations for more Rust `std` types, or why not supporting `async` functions with [`async-ffi`](https://github.com/oxalica/async-ffi)?!

Furthermore, It’s also nice to give a sneak peek on what others do for comparison: **OCaml** [allows extensions to be written directly in Rust with no C stubs](https://docs.rs/ocaml), this work was supported from the [OCaml Software Foundation](http://ocaml-sf.org/) and you can find [a basic example project here](http://github.com/zshipko/ocaml-rust-starter). It offers [safe OCaml/Rust interoperability](https://github.com/simplestaking/ocaml-interop), meaning utilities to convert ADTs (Algebraic Data Types) and function using them.

It would be delightful to get as far as having custom preludes in Haskell binding code generated that offers Rust type layout in Haskell. E.g. Rust [slices](https://doc.rust-lang.org/book/ch04-03-slices.html) are not an existing concept in C but could be easily represented as an FFI-safe struct.

Finally, It’s worth mentioning that there are also proposals to improve the interface between Haskell programs requiring Rust libraries, including this Cabal [RFC](https://github.com/haskell/cabal/issues/7906). As a reminder, the implementation proposed here does not provide support for Haskell dependencies in Rust project yet, but there is a previous [unmaintained attempt](https://github.com/mgattozzi/curryrs) of bringing Haskell runtime support in Rust binaries.

Thanks for reading, feel free to experiment with this Proof of Concept and to provide feedbacks on GitHub :)