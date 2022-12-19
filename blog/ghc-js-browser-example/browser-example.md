
# GHC JavaScript Browser Example

## Introduction


## Building

TODO: Can we do something with the cabal project file and/or environment variables and the with-compiler/with-hs-pkg fields to optionally specify /path/to/build/stage1/bin in the case that the user is currently building the compiler themselves, since Hadrian can't install stage 1 and prebuilt binaries are generally unavailable.

## Features

### Foreign Import JavaScript

To access JavaScript-specific features in Haskell, we can import them via Haskell's `foreign import` syntax.

In our example (in `app/Main.hs`), we have:
```
foreign import javascript unsafe "((html) => { return setInner(html); })"
  setInnerHtml :: CString -> IO ()
```

Here, there are a number of details to take note of:
- we specify the import as a string of JavaScript with _a single function call_ (TODO: Elaborate on syntax)
- _TODO: Explain unsafe vs interruptible_
- we must pass arguments as types from the `base` library's `Foreign.C` module - in this case `CString`
- return types must also be `Foreign.C` types, or unit (`()`)

On the JavaScript side (in `js/example.js`), we have:
```
function setInner(html) {
  document.body.innerHTML = "example" + h$decodeUtf8z(html,0);
  return;
}
```

In this JavaScript function, we match the inputs specified in the `foreign import` declaration. However, we unfortunately have to currently use internal functions of GHC's JavaScript backend - in order to convert this `CString` (stored as a byte array) into a JavaScript string, we use `h$decodeUtf8z`.

## Current Limitations

### Libraries Using C FFI Imports

If a function is imported via `foreign import ccall`, then that symbol will end up being referenced in the generated JavaScript, with `h$` prepended.

In the example code, the `lucid-svg` library indirectly calls `bytestring`'s `cIsValidUtf8`, which imports the c symbol `bytestring_is_valid_utf8`. To work around this, we can add the missing definition in our own javascript - which we do in `js/example.js`:

```
function h$bytestring_is_valid_utf8(bs) {
  return true;
}
```

Although this definiton is far from ideal, it demonstrates that we can link in missing library c functions via our own implementation in our project.

### TODO: Talk about the ghc-options JS source linking workaround?
