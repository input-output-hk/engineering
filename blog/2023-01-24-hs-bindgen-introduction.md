---
slug: 2023-01-24-hs-bindgen-introduction
title: One step forward, an easier interoperability between Rust and Haskell
authors: [yvan]
tags: [cabal, rust, bindgen]
---

**TL;DR:** This blog post intends to sum up the why and how of [`cargo-cabal`](https://github.com/yvan-sraka/cargo-cabal) and [`hs-bindgen`](https://github.com/yvan-sraka/hs-bindgen). If you’re looking for usage walkthroughs and code examples, check out project READMEs on GitHub!

> **N.B.** quoted paragraphs in this article give straightforward motivation regarding some systems programming basic concepts. Feel free to skip them if you know you’re likely to be already comfortable with them ;)

## Context

At IOG we maintain large [Haskell](https://www.haskell.org) codebases and we would like to interface them with some libraries written in [Rust](https://www.rust-lang.org).

Rust is a system programming language known for its strong static typing guarantees, which make it similar to Haskell (even if a bit less expressive). However, unlike Haskell, Rust does not have a GC (Garbage Collector) and uses a compile-time memory management strategy. This mechanism is encoded through its type system with the concepts of _“ownership”_ and _“lifetime”_ of values, which can complicate the writing of programs but unlock smaller runtime costs footprint. Rust is becoming increasingly popular in the systems programming and embedded systems domains, and it is also used in areas such as cryptography, where performance and correctness are critical.

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

* From **C** to Rust [`rust-bindgen`](https://github.com/rust-lang/rust-bindgen) ;
* From Rust to **C** [`c-bindgen`](https://github.com/eqrion/cbindgen) and Rust to **ECMAScript**, [`wasm-bindgen`](https://github.com/rustwasm/wasm-bindgen) ;
* Both from and to Rust with **C++** [`cxx`](https://github.com/dtolnay/cxx) and **Python** [`PyO3`](https://github.com/PyO3/pyo3).

This list isn't exhaustive but give you a hint, all these projects are about generating bindings (bindgen)!

> **Why bindgen (bindings code generation)?**
>
> Let's sum it up by: _"A good FFI is an FFI that you don't write …"_
>
> FFI are like a blind spot in your type system. Writing them manually is both frankly painful and really dangerous, as your compiler will not warn you about non-matching interfaces.
>
> Binding generation comes to the rescue by considerably reducing the room for human errors. As a bonus, it also makes maintainers' life easier thanks to a smaller and more readable code base.

## Example

So, let's look at a minimal example. In this case, if we annotate a function like this:

```rust
use hs_bindgen::*;

#[hs_bindgen(CString -> IO ())]
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

#[no_mangle] // Mangling makes symbol names more difficult to predict.
             // We disable it to ensure that the resulting symbol is really `__c_greetings`.
extern "C" fn __c_greetings(__0: *const core::ffi::c_char) -> () {
    // `traits` module is `hs-bindgen::hs-bindgen-traits`
    // n.b. do not forget to import it, e.g., with `use hs-bindgen::*`
    traits::ReprC::from(greetings(traits::ReprRust::from(__0),))
}
```

… and will also generate the following Haskell code:

```haskell
-- This file was generated by `hs-bindgen` crate and contains C FFI bindings
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
--   this format has purposely no public specification).

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
> First, GHC currently doesn't know anything about Rust calling convention, while it does about C's one: C's calling convention is the [_lingua franca_](https://en.wikipedia.org/wiki/Mediterranean_Lingua_Franca) of `rustc`/`ghc`.
> 
> Additionally, the Rust [ABI](https://doc.rust-lang.org/reference/items/external-blocks.html#abi) (call-convention and types [memory layout](https://cheats.rs/#memory-layout)) isn’t stable. That means that it’s specified internally but could be broken by any `rustc` minor release, building a software on top of it is by definition a “hack” … If we think it’s worth it, we would have to perform our bindgen against a given `rustc` version (and that would be really laborious to maintain). So, do not fear the C ABI because, at least, it is stable!
>
> **To go further:** I invite you to read *[“Rust does not have a stable ABI”](https://viruta.org/rust-stable-abi.html)* by Federico Mena Quintero: a blog post discussing how much the absence of Rust stable ABI isn't a big deal in the context of GTK development. Highlighting that *[“How Swift Achieved Dynamic Linking Where Rust Couldn't”](https://faultlore.com/blah/swift-abi/)* by Aria Beingessner isn't so far from [`GObject` Introspection](https://gi.readthedocs.io/en/latest/) strategy!

## Implementation

The previous code example highlighted that we rely on two constructs: an attribute procedural macro [`#[hs_bindgen]`](https://github.com/yvan-sraka/hs-bindgen), and (internally) `ReprRust` and `ReprC` traits.

> **Why use a Rust macro?**
>
> Binding code generation could have been achieved using an external tool, e.g., `c-bindgen` parses Rust code (before macro expansion) and deduces the right C function signatures.
>
> But instead we decided to define a custom macro (like `cxx`, `wasm-bindgen`, and `PyO3` do), and so we require the user to depend on a custom crate.
>
> The reason is that we want generated bindings to always match the source code used for their generation. By using a macro we enforce binding generation during the build process and bindings can't get out-of-sync.
>
> The natural way to do that in Rust, and to have directly access to source code's AST (Abstract Syntax Tree), rather than trying to implement a parser, is to use a macro!
>
> **To go further:** `c-bindgen` is well known to be buggy! It's a flaw in its design to have a custom Rust source parser, rather using the framework implement here for `hs-bindgen` would allow us to provide a better `c-bindgen` implementation.

Wrapping user types by these traits have several benefits:

* Unsupported types are nicely reported as _“the trait `ReprRust<T>` is not implemented for `U`”_ error (that suggest other types that the trait implement to the user);

* The user can extensively always implement these traits for arbitrary types ;

* Provided traits implementation for `std` types take care of memory management ;

* Traits improve a lot of ergonomics by implicitly and safely casting a given type to an FFI-safe one.

> **What's an FFI-safe type?**
>
> `rustc` will complain if a function prefixed by `extern` keyword use as arguments types that are not FFI-safe. FFI-safe types guarantee that a type has a [specified layout](https://doc.rust-lang.org/reference/type-layout.html) (memory representation) by e.g. having a [`#[repr(C)]`](https://doc.rust-lang.org/nomicon/other-reprs.html#reprc) compiler attribute, for the given C call convention.

**To go further:** the memory management strategy is freeing the value is the role of the receiver (which has “ownership” of it). This means that values returned by Rust functions aren't [`dropped`](https://doc.rust-lang.org/std/ops/trait.Drop.html) by Rust but rather should be [`freed`](https://hackage.haskell.org/package/base/docs/Foreign-Marshal-Alloc.html) on the Haskell side!

Currently `hs-bindgen` only generates `unsafe` Haskell foreign imports. In the future it could generate `safe` ones too. I invite you to read *[“FFI safety and GC”](https://frasertweedale.github.io/blog-fp/posts/2022-09-23-ffi-safety-and-gc.html)* by Fraser Tweedale or GHC's users guide to understand the differences between `unsafe`/`safe`.

## DevX

[`cargo-cabal`](https://github.com/yvan-sraka/cargo-cabal) is a CLI tool that helps you, in one simple command, turning a Rust crate into a Haskell Cabal library!

I was heavily inspired by the developer experience that offers [`wasm-pack`](https://rustwasm.github.io/docs/wasm-pack/) or [`maturin`](https://github.com/PyO3/maturin): launched in any Rust project folder, it helps the user to interactively tweak their `Cargo.toml` to make it successfully build a package compatible with NPM / PyPI and even handle the publishing of it.

What `cargo-cabal` actually does is:

* Ask the user to add `crate-type = ["staticlib"]` (or `"cdylib"`, dynamic libraries require an extra [`build.rs`](https://github.com/yvan-sraka/cargo-cabal/blob/main/src/build.rs) file that is generated by `cargo-cabal`) to their `Cargo.toml` file;

* Generate a custom `X.cabal` linking `rustc` output as `extra-librairies`, and either a ([`naersk`](https://github.com/nix-community/naersk) and [`haskell.nix`](https://github.com/input-output-hk/haskell.nix) based) `flake.nix` or a [`Setup.lhs`](https://github.com/yvan-sraka/cargo-cabal/blob/main/src/Setup.lhs) Cabal build script (to work around this [issue](https://github.com/haskell/cabal/issues/2641)).

**To go further:** `stack` isn't supported yet, but we could easily imagine a `cargo-stack` binary that just wraps a `cargo-cabal --stack` CLI option!

## What's next?

[`cargo-cabal`](https://github.com/yvan-sraka/cargo-cabal) and [`hs-bindgen`](https://github.com/yvan-sraka/hs-bindgen) combined are less than 1000 LoC, they also support Rust `#[no_std]` code, and I would be glad to keep them as [KISS](https://en.wikipedia.org/wiki/KISS_principle) and modular as possible. But there is still room for improvements, e.g. by adding traits' implementations for more Rust `std` types, or why not supporting `async` functions with [`async-ffi`](https://github.com/oxalica/async-ffi)?!

Furthermore, It’s also nice to give a sneak peek on what others do for comparison: **OCaml** [allows extensions to be written directly in Rust with no C stubs](https://docs.rs/ocaml), this work was supported from the [OCaml Software Foundation](http://ocaml-sf.org/) and you can find [a basic example project here](http://github.com/zshipko/ocaml-rust-starter). It offers [safe OCaml/Rust interoperability](https://github.com/simplestaking/ocaml-interop), meaning utilities to convert ADTs (Algebraic Data Types) and functions using them.

It would be delightful to get as far as having custom preludes in Haskell binding code generated that offers Rust type layout in Haskell. E.g. Rust [slices](https://doc.rust-lang.org/book/ch04-03-slices.html) are not an existing concept in C but could be easily represented as an FFI-safe struct.

Finally, it’s worth mentioning that there are also proposals to improve the interface between Haskell programs requiring Rust libraries, including this Cabal [RFC](https://github.com/haskell/cabal/issues/7906). As a reminder, the implementation proposed here does not provide support for Haskell dependencies in Rust project yet, but there is a previous [unmaintained attempt](https://github.com/mgattozzi/curryrs) by Michael Gattozzi of bringing Haskell runtime support in Rust binaries. We should also keep a close look on the Rust RFC that offers to introduce a `#[repr(interop)]` attribute: [Experimental feature gate proposal `interoperable_abi`](https://github.com/rust-lang/rust/pull/105586).

I would like to thank [@hsyl20](https://github.com/hsyl20), [@iquerejeta](https://github.com/iquerejeta) and [@govanify](https://github.com/govanify) for their reviews and for their helpful suggestions.

Thanks for reading, feel free to experiment with this Proof of Concept and to provide feedback on GitHub!
