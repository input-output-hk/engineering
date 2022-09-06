# JavaScript Modules for GHCJS

The addition of the modules feature to JavaScript (a.k.a. "ECMAScript modules") offers the possibility of using these for GHCJS code generation. This document presents an analysis for this purpose.

JavaScript modules allow files to be written _with proper scoping rules_ - something previously left to a hack using anonymous functions.

### The Anonymous Function Module Pattern

In JavaScript, without the ECMAScript modules feature, the accepted method to write properly scoped files is to define an object within an anonymous function (of no arguments), return that object, and immediately apply that function - storing the returned object in a variable named for the module. Because the 'module' definition is written inside a function, symbols don't escape the scope. With this, we can ensure that exported symbols don't clash with other modules. We can also define variables that are local to the module, and statically retain their values (similar to static class members in Java). So, an example of this pattern might look like:

```javascript
// MyModule.js

MyModule = (function() {

  var moduleExports = {};

  var aStaticVar = "";

  moduleExports.append = function(suffix) {
    aStaticVar += suffix;
  }

  moduleExports.recall = function() {
    return aStaticVar;
  }

  return moduleExports;

})(); // The anonymous function is immediately applied, returning the module as an object.

```

Then, to use these from another file we can either use the functions directly (using object access syntax), or bind it to a variable first, which can be done per function or using destructuring syntax:

```javascript
// Main.js

// Direct usage:
MyModule.append("example");

// Variable binding:
var f = MyModule.append;
f("example");

// Destructuring:
let { append: f, recall: g } = MyModule;
f("example");

```

Finally, to tie these together, we use the `script` tag in HTML. Because the `script` tag essentially pastes the contents of file in-place, we must take care to include files before their modules are used.

```html
<html>
  <head>
    <script type="text/javascript" src="MyModule.js"></script> <!-- MyModule.js is included before it is used in Main.js -->
    <script type="text/javascript" src="Main.js"></script>
  </head>
  <body></body>
</html>
```

## ECMAScript/JavaScript Modules

To properly support module scoping, ECMAScript modules were added to the JavaScript standard. With these, we no longer have to wrap the file in an anonymous function. Instead, exported symbols are marked with the `export` keyword - which can be done either on each exported definition, or once at the end of the file. Optionally, as a convention, JavaScript files can use the `.mjs` extension, so the previous `MyModule` example would be defined as:

```javascript
// MyModule.mjs

var aStaticVar = "";

export function append(suffix) {
  aStaticVar += suffix;
}

export function recall() {
  return aStaticVar;
}

// A single export statement can be used _instead of_ marking each definition with `export`:
export { append, recall };

```

Then, to use this module in another, an `import` statement is used. Optionally, names can be changed on import (and export) using the `as` keyword:

```javascript
// Main.mjs
import { append as f } from './MyModule.mjs';

f("example");
```

Now, we can use this in HTML by referencing just the top-level module (here `Main`) that we want, and all of its imported modules are automatically loaded - without having to worry about include order. To let the browser know that we're using the modules feature, we have to specify `type="module"` in the `script` tag, instead of `type="text/javascript"`.

```html
<html>
  <head>
    <!-- MyModule.mjs doesn't need to be explicitly imported in HTML - it's already imported in Main.mjs -->
    <script type="module" src="Main.mjs"></script>
  </head>
  <body></body>
</html>
```

There are a number of scoping rules that offer advantages over using anonymous functions:
* Modules imported into other modules don't need to be referenced in the HTML
* Modules can be imported multiple times, and the engine will deal with this automatically - only requesting the file once from the server
* Imported symbols are only available in the modules that import them, so the global namespace isn't polluted.

## ECMAScript Module Imports in GHCJS

In GHCJS, we could use ECMAScript modules for a form of on-the-fly linking. However, this wouldn't offer any of the compacting features afforded by the GHCJS linker, and each module would require a separate web request - limiting distribution efficiency.

Because of this, the use of ECMAScript module imports in generated code should be limited to the borders of libraries. In particular, we could use import statements in generated code to join large JavaScript libraries with our generated JavaScript. Depending on performance characteristics, which would need to be benchmarked, it could also be used for foreign imports of user code.

External bundling tools, when combined with the use of import statements, might allow for the replacement the specialised GHCJS linker.

## ECMAScript Module Exports in GHCJS

In contrast to imports, exports seem to offer a more immediate use case in code generation. For example, we could _always_ generate the final JavaScript file (being the Haskell Main module for executables, and otherwise for libraries) as an ECMAScript module, and mark every (Haskell or foreign) exported symbol with the JavaScript `export` keyword. In the case that we want to do this for all Haskell exported symbols (using the `module Main (...) where` syntax), care would have to be taken to ensure that the generated code calls into the runtime correctly, even when the symbol is referenced from JavaScript.

So, we might have a set of Haskell modules:

```haskell
-- Lib.hs
module Lib (f, g) where

f :: Int -> Int
f x = x + 1

g :: Int -> Int
g x = x - 1

h :: Int -> Int
h x = x * 2

-- Main.hs
module Main where

import Lib (f)

main :: IO ()
main = return ()

j :: Int -> Int
j x = j `div` 2
```

And generate code such as:

```javascript
// Main.mjs
export f(x) {...}
export j(x) {...}
export main(/* RealWorld */) {...}
```

In this example of exporting all visible symbols via the JavaScript module syntax, we see that `h` and `g` don't end up in the final JavaScript:
* `h` because it isn't exported from `Lib`
* `g` because it isn't imported into `Main`

Additionally, we see where a `RealWorld` argument could be passed to the `main` action - depending on if the code generation needs it.
