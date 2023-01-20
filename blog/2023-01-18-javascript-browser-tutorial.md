---
slug: 2023-01-18-javascript-browser-tutorial
title: Using GHC's JavaScript Backend in the Browser
authors: [sylvain, doyougnu, luite, josh]
tags: [ghc, javascript, cross-compilation]
---


# Using GHC's JavaScript Backend in the Browser

In a previous
[post](https://engineering.iog.io/2022-12-13-ghc-js-backend-merged) we
introduced GHC's new JavaScript backend, which allows the compilation of Haskell
code into JavaScript. This is the first tutorial in a new series about the
JavaScript backend. We plan to write more of those in the coming weeks and
months as we add new features (e.g. support for "foreign exports" that will
allow JavaScript code to call into Haskell code, support for Template Haskell,
etc.). For now it relies on our "insider" knowledge (e.g. how the FFI works)
that isn't well documented elsewhere. We do plan to add a chapter about the
JavaScript backend in GHC's user guide, but for now your best chance is to look
at GHCJS's documentation or at the source code. In this post, we'll build GHC as
a JavaScript cross-compiler and run a trivial Haskell program in the browser.

Please note: this is a technology preview of the in-development JavaScript backend
for GHC. Not all Haskell features are implemented, and bugs are expected. It is
currently not possible for JavaScript code to call into Haskell code ("foreign
exports" aren't implemented). GHC isn't a multi-target compiler yet, so a GHC executable
built for a native platform (Linux/x86-64, Windows/x86-64, Darwin/AArch64...) as currently distributed (via ghcup, Stack, binary distributions, etc.) won't be able to produce JavaScript.
That's why we start this post with the required steps to build yourself
a GHC compiler capable of producing JavaScript.

## Building GHC as a Cross Compiler to JavaScript

### Installing Dependencies

First we need to install all the typical dependencies for GHC plus `Emscripten`,
so our final list is:

* GHC version 9.2 or later
* Cabal
* Alex
* Happy
* Emscripten to configure with
* (Optional) NodeJS to run JavaScript locally


Let's take these in order, a standard GHC distribution with Cabal is needed so we can boot our new compiler.
We recommend using  GHCUP
([https://www.haskell.org/ghcup/install/](https://www.haskell.org/ghcup/install/)),
or your system's package manager to install the this. 

We need Alex and Happy to build GHC, these can be installed through Cabal:

```
cabal install alex happy -j
```

We need Emscripten during the `configure` step of the build. `Emscripten` should be available in most package managers, but you can also build and install it from source:

```
git clone https://github.com/emscripten-core/emsdk.git
cd emsdk
./emsdk install latest
./emsdk activate latest
source ./emsdk_env.sh
```

After installing Emscripten, `emconfigure` should be available on your system
path. Use `which emconfigure` to check that it is on your `$PATH`. If you built
from source, then the output should point to a location within the emsdk git
project like so:

```
$ which emconfigure
/path/to/emsdk/upstream/escripten/emconfigure
```

For more detailed installation instructions, see [https://emscripten.org/docs/getting_started/downloads.html](https://emscripten.org/docs/getting_started/downloads.html). 

That's all we need to build GHC as a cross compiler. NodeJS can be installed via your system's package manager if you want to run the JavaScript programs locally. We'll assume it's in your `$PATH` for the rest of the blog post.


### Building GHC

With all the dependencies installed, we can clone GHC HEAD and build the cross compiler:
```
git clone https://gitlab.haskell.org/ghc/ghc.git --recursive
```
You should notice quite a few submodules being cloned as well as the main repo; expect this to take a while. Once this has completed, navigate to the `ghc` directory and run the following configuration commands:
```
cd ghc
./boot
emconfigure ./configure --target=js-unknown-ghcjs
```

`emconfigure ./configure --target=js-unknown-ghcjs` will finish by outputting a screen that looks like:
```
----------------------------------------------------------------------
Configure completed successfully.

   Building GHC version  : 9.5.20221219
          Git commit id  : 761c1f49f55afc9a9f290fafb48885c2033069ed

   Build platform        : x86_64-unknown-linux
   Host platform         : x86_64-unknown-linux
   Target platform       : js-unknown-ghcjs

   Bootstrapping using   : /home/josh/.ghcup/bin/ghc
      which is version   : 9.4.2
      with threaded RTS? : YES

   Using (for bootstrapping) : gcc
   Using clang               : /home/josh/emsdk/upstream/emscripten/emcc
      which is version       : 15.0.0
      linker options         :
   Building a cross compiler : YES
   Unregisterised            : NO
   TablesNextToCode          : YES
   Build GMP in tree         : NO
   hs-cpp       : /home/josh/emsdk/upstream/emscripten/emcc
   hs-cpp-flags : -E -undef -traditional -Wno-invalid-pp-token -Wno-unicode -Wno-trigraphs
   ar           : /home/josh/emsdk/upstream/emscripten/emar
   ld           : /home/josh/emsdk/upstream/emscripten/emcc
   nm           : /home/josh/emsdk/upstream/bin/llvm-nm
   objdump      : /usr/bin/objdump
   ranlib       : /home/josh/emsdk/upstream/emscripten/emranlib
   otool        : otool
   install_name_tool : install_name_tool
   windres      :
   dllwrap      :
   genlib       :
   Happy        : /home/josh/.cabal/bin/happy (1.20.0)
   Alex         : /home/josh/.cabal/bin/alex (3.2.7.1)
   sphinx-build :
   xelatex      :
   makeinfo     :
   git          : /usr/bin/git
   cabal-install : /home/josh/.cabal/bin/cabal

   Using LLVM tools
      clang : clang
      llc   : llc-14
      opt   : opt-14

   HsColour was not found; documentation will not contain source links

   Tools to build Sphinx HTML documentation available: NO
   Tools to build Sphinx PDF documentation available: NO
   Tools to build Sphinx INFO documentation available: NO
----------------------------------------------------------------------
```

If everything is correct, you'll see that the `Target platform` is set to
`js-unknown-ghcjs`, and the build tools will be set to their
Emscripten counterparts: `ar` becomes `emar`, `nm` becomes `llvm-nm`, etc.

Finally, to build GHC:
```
./hadrian/build --bignum=native -j --docs=none
```

Expect this to take around a half hour or longer. If all goes well you should see:
```
/--------------------------------------------------------\
| Successfully built library 'ghc' (Stage1, way p).      |
| Library: _build/stage1/compiler/build/libHSghc-9.5_p.a |
| Library synopsis: The GHC API.                         |
\--------------------------------------------------------/
| Copy package 'ghc'
# cabal-copy (for _build/stage1/lib/package.conf.d/ghc-9.5.conf)
| Run GhcPkg Recache Stage1: none => none
| Copy file: _build/stage0/bin/js-unknown-ghcjs-ghc => _build/stage1/bin/js-unknown-ghcjs-ghc
Build completed in 1h00m
```

Take note of `_build/stage1/bin/js-unknown-ghcjs-ghc` path. This is the GHC executable that we'll use to compile to JavaScript. To make life easier on ourselves we can alias it:

```
alias ghc-js=`pwd`/_build/stage1/bin/js-unknown-ghcjs-ghc
```

## First Haskell to JavaScript Program

Now that we have a version of GHC that can output JavaScript, let's compile a Haskell program and run it with NodeJS. Make a file named "HelloJS.hs", with the following contents:

```haskell
-- HelloJS.hs
module Main where

main :: IO ()
main = putStrLn "Hello, JavaScript!"
```

Now we can compile it using the alias we defined earlier:
```
ghc-js HelloJS.hs
```

You should see the following output, and a `HelloJS` executable.

```
[1 of 2] Compiling Main             ( HelloJS.hs, HelloJS.o )
[2 of 2] Linking HelloJS.jsexe
```

If you have NodeJS is on your `Path`, then this executable can be run just like any other command line program:

```
./HelloJS
Hello, JavaScript!
```

Notice that a directory called `HelloJS.jsexe` was created. This directory
contains all the final JavaScript code, including a file named `all.js`, and a
minimal `index.html` HTML file that wraps `all.js`. For now, we'll only care
about `all.js` and return to `index.html later. `all.js` _is_ the payload of our
`HelloJS` exectuable. The executable is simply a copy of `all.js`, with a call
to `node` added to the top. We could have equivalently run our program with:

```
node HelloJS.jsexe/all.js
```

## Haskell in the Browser

We saw in the previous example that GHC's JavaScript backend allows us to write
Haskell and run the output JavaScript with NodeJS. This produces a portable
executable, but otherwise doesn't enable anything we couldn't do before; GHC can
already compile Haskell to run on most platforms! So let's do something novel,
and run Haskell in the browser.

In this example, we'll use Haskell to draw a simple SVG circle to our browser window. Put the following code in a file named `HelloBrowser.hs`:

```haskell
-- HelloBrowser.hs
module Main where

import Foreign.C.String

foreign import javascript "((arr,offset) => document.body.innerHTML = h$decodeUtf8z(arr,offset))"
  setInnerHtml :: CString -> IO ()

circle :: String
circle = "<svg width=300 height=300><circle cx=50% cy=50% r=50%></circle></svg>"

main :: IO ()
main = withCString circle setInnerHtml
```

Notice that we've encountered a Haskell feature that's only available in the
JavaScript backend: JavaScript foreign imports. This feature allows our Haskell
program to call JavaScript functions. In our example we use this feature to call
a JavaScript [arrow
function](https://262.ecma-international.org/13.0/#prod-ArrowFunction) that
updates the `body` of the page with our HTML snippet containing a drawing of a
circle. Alternatively, we could have set the foreign import to a function symbol
like so:

```
foreign import javascript "setInnerHTML"
  setInnerHtml :: CString -> IO ()
```

where `setInnerHTML` is defined in a `.js` file that is then loaded
by passing the JavaScript file to GHC along with the Haskell sources.

Next, we can compile our program to JavaScript, again with our built GHC:

```
ghc-js HelloBrowser.hs
```

Or `ghc-js HelloBrowser.hs foo.js` if `setInnerHTML` is defined in `foo.js`.


Recall the `index.html` file inside the `HelloBrowser.jsexe` directory. This HTML
file has our compiled JavaScript already included, so if you open it in your
browser, you'll find it loads our SVG circle in the top-left of the page!

![Example webpage screenshot](/img/browser-screenshot.png)

It's also possible to use our program with existing HTML. In `index.html`, you'll find the line:
```html
<script language="javascript" src="all.js" defer></script>
```
This references the `all.js` file that we described in the first example. So, if we had a HTML document with content, and we wanted to modify it via Haskell, we'd just have to include our program with the `script` tag! 

## Conclusion

In this post, we've seen how to build a first Haskell program to run in the browser using a preview of GHC's in-development JavaScript backend. This program used "foreign imports" to make a JavaScript function available within the Haskell code, which allows a limited interaction between Haskell and the browser. We also saw the structure of the outputs of the JavaScript backend, in the `.jsexe` directory, and how this allows our Haskell program to be invoked by a custom HTML wrapper. This was all enabled by building a version of GHC from source, with the build process having been configured with Emscripten to produce a GHC exectuable that targets JavaScript.
