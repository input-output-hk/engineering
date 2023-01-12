
# Using GHC's JavaScript Backend in the Browser

In the our last major
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

## Building GHC as a Cross Compiler to JavaScript

### Installing Dependencies

First we need to install all the typical dependencies for GHC plus `Emscripten`,
thus the final list is:

* A GHC/Cabal to boot with
* Alex
* Happy
* Emscripten to configure with
* (Optional) NodeJS to run JavaScript locally


A standard GHC distribution with Cabal is required. This is best installed via GHCUP ([https://www.haskell.org/ghcup/install/](https://www.haskell.org/ghcup/install/)), or your system's package manager. As of writing, a GHC version of 9.2 or later is required. 

Now, we'll also need a couple of Haskell programs, which we'll install through Cabal.

```
cabal install alex happy -j
```

We'll be using Emscripten during the `configure` step - which is often available in package managers - but can also be installed from source:

```
git clone https://github.com/emscripten-core/emsdk.git
cd emsdk
./emsdk install latest
./emsdk activate latest
source ./emsdk_env.sh
```

After installing Emscripten, `emconfigure` should be available on your system path. If the installation was successful, `which emconfigure` should point to a location within the emsdk git project, for example:

```
$ which emconfigure
/path/to/emsdk/upstream/escripten/emconfigure
```

For more detailed installation instructions, see [https://emscripten.org/docs/getting_started/downloads.html](https://emscripten.org/docs/getting_started/downloads.html). 

That's all we need to build GHC as a cross compiler. NodeJS can be installed via your system's package manager if you want to run the JavaScript programs locally. We'll assume its in your `$PATH` for the rest of the blog post.


### Building GHC

With all the dependencies installed, we can clone GHC HEAD and build the cross compiler:
```
git clone https://gitlab.haskell.org/ghc/ghc.git --recursive
```
You should notice quite a few submodules being cloned as well as the main repo; expect this to take a while. Once this has completed, change into the `ghc` directory, where we can run some configuration commands:
```
cd ghc
./boot
emconfigure ./configure --target=js-unknown-ghcjs
```

`configure` will finish by outputting a screen that looks like:
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
`js-unknown-ghcjs`, and the tools needed to build will be set to their
Emscripten counterparts - `ar` becomes `emar`, `nm` becomes `llvm-nm`, etc.

Finally, to build GHC:
```
./hadrian/build --bignum=native -j --docs=none
```

Which will result in:
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

Take note of `_build/stage1/bin/js-unknown-ghcjs-ghc` path. This is the GHC executable that we'll be using to compile to JavaScript. To make life easier on ourselves we can alias it:
```
alias ghc-js=`pwd`/_build/stage1/bin/js-unknown-ghcjs-ghc
```

## First Haskell to JavaScript Program

Now that we have a version of GHC that can output JavaScript, let's move on to using it to compile a Haskell program, which we can run using NodeJS. Make a file named "HelloJS.hs", with the following contents:

```haskell
-- HelloJS.hs
module Main where

main :: IO ()
main = putStrLn "Hello, JavaScript!"
```

Then, to compile it to JavaScript, we'll need the path to the new GHC executable we just built. To run it, use:
```
ghc-js HelloJS.hs
```

You should see the following output, and that it has produced a `HelloJS` executable.

```
[1 of 2] Compiling Main             ( HelloJS.hs, HelloJS.o )
[2 of 2] Linking HelloJS.jsexe
```

If you have NodeJS installed, this executable can be run just like any other command line program, which will output `Hello, JavaScript` to the console:

```
./HelloJS
```

Notice that a folder called `HelloJS.jsexe` was produced. This directory contains all the final JavaScript code in it, including a file named `all.js`, and a minimal `index.html` HTML file that wraps `all.js`. For now, we'll only care about `all.js` and return to `index.html later. `all.js` _is_ the payload of our `HelloJS` exectuable. The executable is simply a copy of `all.js`, with a call to `node` added to the top. So, we can also run our program with:

```
node HelloJS.jsexe/all.js
```

## Haskell in the Browser

We saw in the previous example that the GHC's JavaScript backend allows us to write Haskell to run with NodeJS. This produces a portable executable, but otherwise doesn't enable anything we couldn't do before - GHC can already compile Haskell to run on most platforms! So, we'll present a unique possibility: running Haskell in the browser.

In this example, we'll use Haskell to draw a simple SVG circle to our browser window. Put the following code in a file named `HelloBrowser.hs`:

```haskell
-- HelloBrowser.hs
module Main where

import Foreign.C.String

foreign import javascript "((html) => document.body.innerHTML = h$decodeUtf8z(html,0))"
  setInnerHtml :: CString -> IO ()

circle :: String
circle = "<svg width=300 height=300><circle cx=50% cy=50% r=50%></circle></svg>"

main :: IO ()
main = withCString circle setInnerHtml
```

Then, we can compile it to JavaScript, again with our built GHC:
```
ghc-js HelloBrowser.hs
```

Now, inside the HelloBrowser.jsexe folder, there will be an `index.html` file. This HTML file has our compiled JavaScript already included, so if you open it in your browser, you'll find it loads our SVG circle!

It's also possible to use our program with existing HTML. In `index.html`, you'll find the line:
```html
<script language="javascript" src="all.js" defer></script>
```
This references the `all.js` file that we talked about in the first example. So, if we had a HTML document with content, and we wanted to modify it via Haskell, we'd just have to include our program with the `script` tag!

In this example we've encountered a Haskell feature that's only available in the JavaScript backend - JavaScript foreign imports. This feature allows us to write JavaScript [arrow functions](https://262.ecma-international.org/13.0/#prod-ArrowFunction) for use in our Haskell program. Here, it's allowed us to write a function to access the `body` of the HTML, and replace its contents with our SVG string.

## Conclusion

In this post, we've seen how to build and use a version of GHC that supports compiling to JavaScript, which allowed us to compile some first programs to run on NodeJS and the browser. We've also taken the first steps in using Haskell's foreign function interface to allow browser-specific features to be accessed in our Haskell programs, and we've seen how to include the resulting JavaScript in a custom HTML document.

This is the first tutorial in a series about the JavaScript backend. We plan to write more of these in the coming weeks and months as we add new features (e.g. support for "foreign exports" that will allow JavaScript code to call into Haskell code, support for Template Haskell, etc.). For now it relies on our "insider" knowledge (e.g. how the FFI works) that isn't well documented elsewhere. We do plan to add a chapter about the JavaScript backend in GHC's user guide, but for now your best chance is to look at GHCJS's documentation or at the source code.
