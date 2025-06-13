---
slug: 2023-02-28-weak-references-in-the-javascript-backend
title: Weak References in the JavaScript backend
date: February 28, 2023
authors: [ luite ]
tags: [ghc, javascript, storagemanager ]
---

## Introduction

I recently gave a short presentation on the topic of weak references in the GHC JavaScript backend to the GHC team at IOG. This blog post is a summary of the content.

## Haskell Weak References

The "Stretching the Storage Manager" [ssm][1] paper describes weak references as implemented by GHC. These weak references are available through the `System.Mem.Weak` module. Each weak reference connects a key and a value. The value is kept alive by the weak reference as long as the key is alive. Optionally, weak references can have a finalizer of type `IO ()`, which is run after the key becomes unreachable.

## JavaScript Weak References

JavaScript has weak references on its own, specifically the `WeakMap`. But the functionality is quite different from Haskell's. `WeakMap` is not iterable, its size is not visible and it has no finalizers. Therefore it's impossible to observe when a weak value has become unreachable.

There have been proposals to add finalizers to `WeakMap` but so far they haven't been implemented because they introduce nondeterminism and expose reachability information which could impact security.

Specific JavaScript environments like node.js do have weak references with the required functionality to implement Haskell `Weak#`. On these platforms we could substitute the general purpose `Weak#` implementation, and we could verify consistency between the general purpose implementation and a node.js specific one.

## Checking Reachability

Since we don't have a way to determine which `Weak#` keys have become unreachable, we have to do the opposite: Check which values are still reachable. The general idea is as follows: Every Haskell heap object gets a mark property `m`, which is changed by the rechability checker.

After scanning the whole heap we can determine which `Weak#` keys are still reachable by checking if their mark has been updated.

## Weak Implementation

All Haskell heap objects have an identical object stucture, with an entry function and some data properties. The entry function also contains metadata about the object, for example the constructor tag for data constructors and the arity for functions.

```javascript
// heap object (incomplete)
{ f   // function, entry point
, d1  // any, first data property
, d2  // any, second data property (or indirection to more data)
}
```

To be able to keep track of reachability, we add one property `m` to each object:

```javascript
// heap object (incomplete)
{ f   // function, entry point
, d1  // any, first data property
, d2  // any, second data property (or indirection to more data)
, m   // number, garbage collection mark
}
```

The mark gets updated by the code that checks for reachability of everything. This means that we could implement a `Weak#` as follows:

```javascript
// weak (not actual)
h$Weak {
    key: heap object
,   value: heap object
,   finalizer: null or heap object
}
```

But this means that our `h$Weak` keeps both the key and the value alive. For the operations that `Weak#` needs to support, this isn't necessary, and in fact we'd like to avoid it so that the JavaScript storage manager can reclaim memory as quickly as possib. That's why we make another change to the heap objects, adding an optional indirection to the mark:

```javascript
// heap object (actual)
{ f   // function, entry point
, d1  // any, first data property
, d2  // any, second data property (or indirection to more data)
, m   // number/h$StableName, garbage collection mark
}

h$StableName {
    stableNameNo: number, unique identifier
,   m           : number, garbage collection mark
}
```

Now we can replace a `number` mark by an `h$StableName` for the key, and then create the weak reference as follows:

```javascript
// weak (actual)
h$Weak {
    key: h$StableName, the stablename of the key heap object
,   value: heap object
,   finalizer: null or heap object
}
```
This way the `h$Weak` does not reference they key itself. It still knows when the key is unreachable, since the mark of the `h$StableName` of the key would not be updated anymore.

## Finalizers

Every time the heap is scanned for dead weak references, the associated finalizers are collected. After the pass, if at least one finalizer needs to be run, the storage manager schedules a new thread. This thread runs all the finalizers of the pass. Exceptions are handled between finalizers, but a finalizer that takes a long time will delay execution of the others.

## Conclusion

We have seen the implementation of weak references in the JavaScript backend. Since we cannot use the JavaScript engine to determine which Haskell heap objects are reachable we use a custom reachability check to implement the required functionality. We have chosen the implmentation in such a way that the JavaScript engine retains as little memory as possible.


[1]: Peyton Jones, Simon and Marlow, Simon and Elliott, Conal, Stretching the storage manager: weak pointers and stable names in Haskell, Proceedings of the 11th International Workshop on the Implementation of Functional Languages, 1999, https://www.microsoft.com/en-us/research/publication/stretching-the-storage-manager-weak-pointers-and-stable-names-in-haskell/