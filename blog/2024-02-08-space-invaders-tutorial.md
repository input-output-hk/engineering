---
slug: 2024-02-08-space-invaders-tutorial
title: Porting a Haskell Game to the JavaScript Backend
authors: [josh]
tags: [GHC JavaScript, tutorial]

[SpaceInvaders](https://github.com/ivanperez-keera/SpaceInvaders) on GitHub is a demo game written by user [Ivan Perez](https://ivanperez.io/) to demonstrate Haskell's [Yampa](https://hackage.haskell.org/package/Yampa) library. It was recently ported by [AntanasKal](https://github.com/AntanasKal/SpaceInvaders) to work in the browser using the WASM backend.

The WebAssembly port motivated us to try porting it to JavaScript using GHC's JavaScript backend that our team developed. The translation from the Wasm backend to JavaScript turned out to be straightforward, and a branch with the complete set of changes can be found on [GitHub](https://github.com/JoshMeredith/SpaceInvaders/tree/wip/js-backend). The port only requires a minimal HTML file to define the canvas that we'll be drawing to, as well as some JavaScript glue code to provide canvas drawing functions and mouse events via the Haskell FFI. The project provides a good end-to-end example of using the JavaScript backend in the browser, and in this post will explain how it works.

## Background Knowledge

The Yampa library in Haskell is based on Functional Reactive Programming (FRP) for modelling programs as events and time-varying values. FRP is useful in the domain of UIs and games because the encoding of events helps with handling user inputs and outputs.

For this tutorial, we'll assume that the main game logic in Yampa is the same across backends, and that our main concern is in feeding values in and out of the edges of this logic.

To accept values out of the game logic so that we can use them in our JavaScript rendering functions, we'll use the `reactInit` function from Yampa. It has the following type:

```haskell
reactInit :: IO a -> (ReactHandle a b -> Bool -> b -> IO Bool) -> SF a b -> IO (ReactHandle a b)
```

The first value to `reactInit` initialises the input state, `a`.

The second argument to `reactInit` is a function that receives the current game state, `b`, on each frame and can perform arbitrary `IO`. For our usage here, we don't need the other arguments passed to our function - we're just rendering the game state in `IO`.

The third argument to `reactInit` is an `SF a b` - a "signal function" that converts Yampa values from `a` to `b`. This is the game logic, which for the purposes of this tutorial we'll just treat as the FRP equivalent to a function of type `a -> b`.

The value that `reactInit` returns is a `ReactHandle a b`. We'll use it like a channel that we can write input values of type `a` to. For this, Yampa's `react` function is available:

```haskell
react :: ReactHandle a b -> (DTime, Maybe a) -> IO Bool
```

In the second argument to `react`, we pass time delta for the frame, paired with an optional input value. We can wrap `react` partially applied to the `ReactHandle` we got from `reactInit` and export the wrapped function to JavaScript for inputting values from the browser.

## Exporting `runGameStep` to JavaScript

To input values for the mouse location and button state from JavaScript, we wrap a call to `react` with the `gameReactHandle` that `reactInit` returns. In this case, `gameReactHandle` is a top level binding because the call to `reactInit` uses `unsafePerformIO`. This is done to share code with the Wasm implementation, but in principle it's possible to define `runGameStep` as a local binding in `main` to avoid using `unsafePerformIO`. For a program only targetting the JavaScript backend, the local binding method would be preferred, and an example of how to do this will be provided later.

```haskell
gameReactHandle :: ReactHandle WinInput (Score, [ObsObjState])
gameReactHandle = unsafePerformIO $ do
    let g = mkStdGen 123
    reactInit
        (pure $ WinInput 0.0 0.0 False)
        actuate
        (parseWinInput >>> restartingGame g)

runGameStep :: Double -> Double -> Bool -> Double -> IO ()
runGameStep x y pressed deltaTime = do
  _ <- react gameReactHandle (deltaTime, Just (WinInput x y pressed))
  return ()
```

With the function we want to export defined, we need to prepare some JavaScript code to receive the exported Haskell function. We declare a global `var runGameStep_` to store the exported function, and a JavaScript function `setRunGameStep(fn)` that sets the global.


```javascript
var runGameStep_;

function setRunGameStep(fn) {
  runGameStep_ = fn;
}
```

Then, `setRunGameStep` is foreign imported into Haskell:

```haskell
foreign import javascript "setRunGameStep" setGameStep
  :: Callback (JSVal -> IO ()) -> IO ()

```

We'll also need a JavaScript function to call the callback, which we'll also name `runGameStep`. In this function, we package the arguments into a single `JSVal` to be given to our exported Haskell function. `JSVal` is just a Haskell type used to refer to untyped JavaScript data that isn't managed by GHC's runtime. We have to package the arguments instead of passing multiple `JSVal`s because `base` (currently) only defines `Callback`s up to 3 arguments. The exact format of the package isn't too important - we just need to match it later when we unpack the data.


```javascript
function runGameStep(x, y, pressed, deltaTime) {
  var args = {
    x: x,
    y: y,
    pressed: pressed,
    deltaTime: deltaTime
  }
  return runGameStep_(args);
}
```

To unpack the arguments, we'll define another JavaScript function and use `RETURN_UBX_TUP4`. Note that this is a c-preprocessor macro, so we'll have to enable this at the top of the file. GHC defines `RETURN_UBX_TUP` from 1 to 10. These macros place their arguments in the GHC runtime and then return from the function with the JavaScript `return` keyword.


```javascript
//#OPTIONS: CPP

function unpackGameStepArgs(args) {
  RETURN_UBX_TUP4(args.x, args.y, args.pressed, args.deltaTime);
}
```

GHC will treat a value returned with a `RETURN_UBX_TUP` macro as an unboxed tuple, so the `UnboxedTuples` and `MagicHash` language extensions are required. Then, we can foreign import `unpackGameStepArgs`. Because `Double` and `Bool` are both base types in the GHC JavaScript runtime, we don't need to do any further conversions for the unpacked arguments. For more information on which types can be converted automatically, and how other types can be marshalled, see https://ghc.gitlab.haskell.org/ghc/doc/users_guide/javascript.html#javascript-ffi-types in the GHC users guide.

```haskell
{-# LANGUAGE MagicHash, UnboxedTuples #-}

foreign import javascript "unpackGameStepArgs" unpackGameStepArgs
  :: JSVal -> (# Double, Double, Bool, Double #)
```

Next, we can write the actual wrapper function that will get exported. Here we use the unboxed tuple syntax to deconstruct the tuple returned by `unpackGameStepArgs` and pass them into `runGameStep`.

```haskell
runGameStep' :: JSVal -> IO ()
runGameStep' args =
  let
    (# x, y, pressed, deltaTime #) = unpackGameStepArgs args
  in
    runGameStep x y pressed deltaTime
```

Finally we can wrap `runGameStep'` into a `Callback` using `syncCallback1`. `runGame` is another foreign imported JavaScript function that starts feeding in values through the exported callback. We'll see how it works in the next section.

```haskell
import GHC.JS.Foreign.Callback

main :: IO ()
main = do
  rgs <- syncCallback1 ThrowWouldBlock runGameStep'
  setGameStep rgs
  runGame
```

To instead write this using local bindings and avoid `unsafePerformIO`, `main` would look like:

```haskell
runGameStep' :: ReactHandle WinInput (Score, [ObsObjState]) -> JSVal -> IO ()
runGameStep' args =
  let
    (# x, y, pressed, deltaTime #) = unpackGameStepArgs args
  in
    runGameStep x y pressed deltaTime

main :: IO ()
main = do

  gameReactHandle <- do
    let g = mkStdGen 123
    reactInit
        (pure $ WinInput 0.0 0.0 False)
        actuate
        (parseWinInput >>> restartingGame g)

  rgs <- syncCallback1 ThrowWouldBlock (runGameStep' gameReactHandle)
  setGameStep rgs
  runGame

```

This isn't done in the Wasm implementation because `runGameStep` is exported using foreign exports, instead of as a `Callback`, which only exists in the JavaScript backend.

```haskell
foreign export ccall runGameStep :: Double -> Double -> Bool -> Double -> IO ()
```

It is potentially possible to store the `gameReactHandle` value elsewhere for Wasm to avoid the use of `unsafePerformIO` with `foreign export`, but that's beyond the scope of this tutorial.

## Feeding in JavaScript Inputs

We now have everything set up to trigger input events in JavaScript that will feed into the game logic implemented in Yampa. Next, we'll need to set up browser event listeners on various mouse actions.

For this, we initialise global variables to store the current mouse location and left button state. Then, we use the built-in JavaScript `document.addEventListener` function to add JavaScript callbacks to the events we want.

```javascript
var mouseX = 0;
var mouseY = 0;
var mouseDown = false;

document.addEventListener('mousemove', (event) => {
    var rect = canvas.getBoundingClientRect();
    mouseX = event.clientX - rect.left;
    mouseY = event.clientY - rect.top;
})

document.addEventListener('mousedown', (event) => {
    mouseDown = true;
})
document.addEventListener('mouseup', (event) => {
    mouseDown = false;
})
```

With the `mouseX`, `mouseY`, and `mouseDown` variables updating automatically, we can then initialise a timer to call our `runGameStep` function regularly using the current values of these global mouse variables. The built-in JavaScript `window.requestAnimationFrame` function takes a callback to call before the screen is next refreshed by the browser. The callback we use here, `step`, just calculates a time delta and passes it and the mouse states into `runGameStep`, before re-calling `window.requestAnimationFrame` on itself for the next frame.

```javascript
function runGame() {
    var previousTimeStamp = null;

    function step(timeStamp) {
        if (!previousTimeStamp) {
            previousTimeStamp = timeStamp;
        }
        const deltaTime = (timeStamp-previousTimeStamp)/1000;
        runGameStep(mouseX, mouseY, mouseDown, deltaTime);
        previousTimeStamp = timeStamp;
        window.requestAnimationFrame(step);
    }
    window.requestAnimationFrame(step);
}
```

Notice that this JavaScript function is named `runGame`. It's the same one that's called at the end of our Haskell `main` function, and is imported into Haskell as a simple `IO ()` action.

```haskell
foreign import javascript unsafe "runGame" runGame :: IO ()
```

For the Wasm backend, this function does some additional setup work to initialise memory buffers before defining `step`. Additionally, it isn't imported into Haskell - instead, the Haskell `main` function is empty, and the Wasm `runGame` function is called immediately after its definition, as a top-level JavaScript statement. This difference is because the JavaScript backend has to do the work to export `runGameStep` as a callback in `main`, so calling `runGame` from JavaScript directly would mean the `runGameStep_` global variable is potentially not set yet, depending on how the JavaScript runtime schedules it.

## Rendering Output Values

Recall that earlier when we called `reactInit` we passed it a function `actuate`. This function gets called each time the game state is updated - usually when `runGameStep` is called. The type of `actuate` is:

```haskell
actuate :: ReactHandle a b -> Bool -> b -> IO Bool
```

For rendering, we just need to use the game state, `b`, and write an `IO` action that calls imported JavaScript code to modify the browser state. In the case of this game, a HTML canvas is used. The canvas element is added in `index.html`:

```html
<canvas id="GameCanvas" width="500" height="500">
```

We'll need a way to address this element from our JavaScript code, so we'll define a global reference to it.

```javascript
const canvas = document.getElementById('GameCanvas');
const context = canvas.getContext('2d');
```

Then all we need to do is define various wrapper functions that operate on the `canvas` and its `context`, and foreign import them into Haskell. We won't go over all of them here, but they'll look like the following:

```javascript
function h$fillRect(x, y, width, height) {
    context.fillRect(x, y, width, height);
}
```

Of note here is that the function name is prepended with `h$`. This is because the JavaScript backend uses that as a prefix for functions imported by `foreign import ccall`. Because the Wasm backend also uses `foreign import ccall`, this allows us to reuse the import code between both backends.

```haskell
foreign import ccall "fillRect" fillRect :: Double -> Double -> Double -> Double -> IO ()
```

In the case of Wasm, functions to be imported into Haskell must be exported from JavaScript in a dictionary:

```javascript
const externalFunctions = {
    // ...

    fillRect : (x, y, width, height) => {
        context.fillRect(x, y, width, height);
    }

    // ...
}
```

## Putting It All Together

Now that we've written all the code to interact with the browser, we can put it into a JavaScript file, which we'll call `jsbits/main.js`. There's no naming requirements for this, and it could be split up into multiple files is preferred.

Then, we can add this to our `.cabal` file using the `js-sources` field, which works similarly to `c-sources`. `js-sources` is supported for library targets from Cabal 3.10, and for executable targets from Cabal 3.12.

```
library
  js-sources: jsbits/main.js
```

or

```
executable SpaceInvadersJS
  js-sources: jsbits/main.js
```

Finally, we can build the project `cabal build all` and find the resulting `.jsexe` folder in Cabal's `dist-newstyle` build directory. The path will look something like `./dist-newstyle/build/javascript-ghcjs/ghc-9.8.1/SpaceInvaders-0.14.2/x/SpaceInvadersJS/build/SpaceInvadersJS/SpaceInvadersJS.jsexe/`. For this particular project, we've named the executable `SpaceInvadersJS` in the Cabal file target. In the `.jsexe` folder is an `all.js` file. This file links everything required to run the project into a single file, including libraries and `js-sources`.

`all.js` also includes a call to start the Haskell `main` function after all of the declarations, so we can just include it as a script at the end of the HTML body. It shouldn't be included earlier than this, because the `main` function might be missing references defined in the HTML file, such as the canvas in this case.


```html
<script src="dist-newstyle/build/javascript-ghcjs/ghc-9.8.1/SpaceInvaders-0.14.2/x/SpaceInvadersJS/build/SpaceInvadersJS/SpaceInvadersJS.jsexe/all.js"></script>
```

## Conclusion

We've seen in this post how to use GHC's JavaScript backend to work with the browser. Doing this involves defining a Haskell function that can input values into the main program loop, which is then converted to a `Callback` using `syncCallback1`. The resulting value is exported into JavaScript by calling foreign imported JavaScript funciton that sets the callback into a global variable.

The exported callback can then be called with changing values of mouse states, that are stored in global variables and updated by browser event callbacks.

States are exported from Haskell back to the browser by calling rendering functions that work on a JavaScript canvas.

This is all included in a JavaScript file that gets linked into the program using Cabal's `js-sources` field.

## Extra Information and Feedback

For more information on working with the JavaScript backend, please see the JavaScript section (https://ghc.gitlab.haskell.org/ghc/doc/users_guide/javascript.html) of the GHC users guide.

If you have any ideas to improve the users guide, feel free to open an issue ticket at https://gitlab.haskell.org/ghc/ghc/-/issues, and, if you know what changes should be made, merge requests are welcome at https://gitlab.haskell.org/ghc/ghc/-/merge_requests using the `documentation` label. Our team (`JoshMeredith`, `hsyl20`, `doyougnu`, and `luite`) will be happy to review any changes and feedback!
