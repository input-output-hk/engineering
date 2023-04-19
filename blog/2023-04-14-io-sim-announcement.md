---
slug: 2023-04-14-io-sim-annoucement
title: IOSim on Hackage!
authors: [coot]
tags: [haskell, io-sim, testing]
custom_edit_url: null

---

## IOSim on Hackage

The IOG Networking Team is pleased to announce that we published [`io-sim`],
[`io-classes`], [`si-timers`], [`strict-stm`], [`strict-mvar`] and
[`io-classes-mtl`] on Hackage.  These are tools without which we could not
imagine writing a complex distributed system like [Cardano].

These packages support our goal of using the same code to run in production and
simulation, what greatly increases the reliability and quality of the final
system.  [`io-sim`] and its ecosystem is designed to let write a simulation
environment which provides provided things usually provided by an operating
system like networking stack or disk IO and develop as well as implement
& model complex applications/systems.

For developing a robust system one needs a proper testing framework which
allows one to model the key characteristics of the system.  To achieve this
goal we needed to create an abstraction that captures the key aspects of the
Haskell runtime and operating system environment for distributed systems.  The
Cardano [network stack][ouroboros-network] is a highly concurrent system, and
as a network application, it needs to deal with time: there are all sorts of
timeouts that guard resource usage: inactivity timeouts, message timeouts, or
an application level `TCP`'s `WAIT_TIMEOUT` among others.  The tools which we
provide permitted us to capture issues related to timing (which abound in
network programming) which, in production, would be extremely rare (things like
simultaneous TCP open or critical race conditions) and ensure that we can test
(in the simulation) these scenarios.  Recently we caught
a [bug][sim-tcp-open-bug] in simultaneous TCP open when one side of the
connection crashed - a corner case of a corner case, that's how effective is
the combination of quickcheck style property-based testing & simulation!

### [IO-Classes][`io-classes`]

[`io-classes`] allow testing production code under simulation where one can
mock services usually provided by the operating system: socket API or disk IO
(both will land on `Hackage` at some point too).  Our design principle was to
closely follow the `base` API but also provide extensions that are packaged as
separate libraries: [`si-timers`], [`strict-stm`], [`io-classes-mtl`].

`io-classes` support `MVar`'s and in the future it will also support
`IOVar`s.  The `IO` instance of `MonadMVar` is using `GHC`'s `MVar`, while
`IOSim` instance is based on a `TVar` specific to `IOSim`.  `MVar`s, whether
native or simulated, provide fairness but don't provide compositionality (like
`TVar`s do via `STM`).  Here's a complete list of currently supported APIs
(more to come in the future, if you need more we are happy to accept
contributions!).  The list includes APIs present in core packages like: `base`,
`stm`, `async` or `time`:

* `MVar` API;
* `stm` API: all monadic operations with support for `TVar`s, `TMVar`s,
  `TQueue`s, `TBQueue`s, `TArray`s, `TSem`s, `TChan`s;
* fork API: `forkIO`, `forkOn`, `forkWithUnmask`, `killThread`, `yield`;
* thread API: `myThreadId`, `labelThread`, `threadStatus`;
* synchronous and asynchronous exceptions API: `throw`, `throwTo`, `catch`,
  `catchJust`, `try`, `tryJust`, `handle`, `handleJust`, `catches`;
  and the high-level `bracket`, `finally`, `onException` and
  `bracketOnException`;
* masking API: `mask`, `mask_`, `uninterruptibleMask`, `uninterruptibleMask_`
* masking state API: `getMaskingState`, `interruptible`, `allowInterrupt`
* `evaluate`
* [`async`] API;
* event trace API: `traceEventIO`, `traceEventMarkerIO`;
* `ST` support: `stToIO`;
* time API: `getCurrentTime`, `getMonotonicTimeNSec`; (more in [`si-timers`])
* timeout API: `registerDelay`, `timeout` ; (more in [`si-timers`])
* delays: `threadDelay`; (more in [`si-timers`])
* although this does not strictly belong to this list, we also support
  `MonadFix`, which at times is useful for both `IO` and `IOSim`!

and non-standard APIs:

* `say` (a `print` like function) 
* labelling `TVar`s & friends (`labelTVar`)
* tracing committed values to `TVar`s and `TMVar`s, inspection of `TVar`s
* race exploration of *IOSimPOR*

We plan to add support for `IORef`s.

There's even more but packaged in separate packages, keep reading!


### [SI-Timers][`si-timers`]

The [`si-timers`] ([SI]) package was designed with two principles in mind:

* provide API which is consistent with `timers` package and thus is using SI
  units (seconds) rather than `Int` represented milliseconds like `base` does;
* provide a safe API for 32-bit systems.

On a 32-bit system an `Int` can represent up to `~35` minutes, which often is
too small for longer delays.

[`si-timers`] also comes with a non-standard polyfill for timers API.  The low
level `base` timers API (e.g. [`registerTimeout`] & friends) is not available
on all the systems (currently not available on `Windows` or `GHCJS`).
Although `GHC` native timers are performant (when available), our polyfill is
only good enough to develop sub-second timeouts.  It's not performant enough on
all platforms for sub-millisecond timeouts as it uses concurrency (on `Windows`
or `GHCJS`) and thus relies on GHC scheduler.  In the future, we are planning
to release an implementation of `timeout` API which is performant across all
platforms (which is part of [`network-mux`] library right now).  There's also
an interesting longer `GHC` project to provide performant native timeouts based
on `io-uring` (Linux) and [`io-completion` ports][io-completion-ports]
(Windows).

### [IOSim][`io-sim`]

[`io-sim`] package provides a pure (free) monad that has instances for type
classes defined in [`io-classses`] and [`si-timers`]. In particular, [`io-sim`]
supports threads (via low-level `forkIO` as well as [`async`] package
interface), deadlock detection, asynchronous exceptions, software transaction
memory (`STM`), lifting `ST` computations, `mfix`, and various APIs that
support tracing and inspection.  

There are many distinctive features of `io-sim`.  It supports time domains,
which means you can emulate isolated services which only communicate through
a simulation of some IPC interface.  The time is discrete and only advances
when all threads are blocked: either because a thread explicitly called
`threadDelay`, or the action it runs blocked one an `STM` transaction.   If
a thread registered a timeout it will be unblocked at a specific time.  Because
the application controls time, this allows testing different interleavings
using the usual QuickCheck techniques.  Here we come to another distinctive
feature of `io-sim`.  John Hughes built an interpreter, called *IOSimPOR*,
which dynamically detects races in a given time slot.  The *IOSimPOR*
interpreter is then able to execute different schedules which revert the order
of evaluation of races and recursively discover & revert new races.  The
implementation follows a partial-order reduction algorithm to limit the
schedule exploration.  _IOSimPOR_ is still considered an experimental feature,
if you encounter a bug we'd really like to hear about it.

### [Strict-STM][`strict-stm`]

The [`strict-stm`] package provides strict versions of `TVar`, `TMVar` & friends.
The `StrictTVar` also provides a way to embed invariants, which allows one to
hook [`nothunks`] api and verify in a testing environment that one never commit
thunks.  [`strict-stm`] and [`nothunks`] were designed after a long development
period of [Cardano] when besides correctness and asymptotic behaviour also
performance started to be important.  In a relatively short time, it allowed to
squash many memory leaks allowing us to release the first version of the current
[Cardano] implementation.

### [Strict-MVar][`strict-mvar`]

The package provides strict version of `MVar`.  Like [`strict-stm`] it will
support a way to embed invariants.

### [IO-Classes-MTL][`io-classes-mtl`]

The [`io-classes`] contain instances only for `ReaderT` monad which are
uncontroversial.  Instances for other monad transformers are packaged in
[`io-classes-mtl`] package.  These are experimental, not properly tested, and
also not extensively used by us at this stage.  Some of the instances are also
novel so please inspect the implementation to get familiar with them before you
start relying on them.

As a design principle, we tried to be compatible with the `exceptions` package
especially when it comes to `MonadThrow` instances of monad transformers.

For `MonadSTM` instances we also transform the associated `STM` monad, e.g. for
`StateT s IO` the `STM (StateT s IO)` monad is `StateT s (STM IO)`.  This means
you can interleave `StateT` operations with `STM`.

We don't provide `MonadAsync` instances for transformer stacks (except of
`ReaderT`).  One could follow [`lifted-async`] or be even more general and
allow monoidal join of the asynchronously computed states.  However, this seems
to extravagant and might lead to subtle concurrency bugs.


## Examples

There are many example usages of the `io-sim` which can be used for
inspiration.  Here we point to just a few which we developed as part of our
testing efforts.

### STM test suite

In the [`Test.Control.Monad.STM`] module we generate valid `STM` terms and evaluate them in
both `IO` and `IOSim`.  This follows the [Composable
memory transactions][stm-paper] paper.

Terms of this mini-language are a GADT:

```hs
data Term (t :: Type) where

    Return    :: Expr t -> Term t
    Throw     :: Expr a -> Term t
    Catch     :: Term t -> Term t -> Term t
    Retry     :: Term t

    ReadTVar  :: Name (TyVar t) -> Term t
    WriteTVar :: Name (TyVar t) -> Expr t -> Term TyUnit
    NewTVar   :: Expr t -> Term (TyVar t)

    -- | This is the ordinary monad bind for STM terms.
    Bind      :: Term a -> Name a -> Term t -> Term t
    OrElse    :: Term t -> Term t -> Term t

-- | expressions which can appear in `Terms`
data Expr (t :: Type) where

    ExprUnit ::           Expr TyUnit
    ExprInt  :: Int    -> Expr TyInt
    ExprName :: Name t -> Expr t
```

Following the rules specified in the [Composable Memory
Transactions][stm-paper] paper we have:
```hs
evalTerm :: Env -> Heap -> Allocs -> Term t -> (NfTerm t, Heap, Allocs)

-- | The heap is a mapping of 'Var's to their current values.
--
newtype Heap = Heap (Map VarId SomeValue)
  deriving (Show, Semigroup, Monoid)

-- | The STM semantics uses two heaps, the other one is called the allocations.
type Allocs = Heap

-- | The normal form for a 'Term' after execution.
--
data NfTerm (t :: Type) where

    NfReturn :: Value t -> NfTerm t
    NfThrow  :: Value a -> NfTerm t
    NfRetry  ::            NfTerm t
```
See [`evalTerm`](https://github.com/input-output-hk/io-sim/blob/93858df930c160450e12f34428b61e74813b14ae/io-sim/test/Test/STM.hs#L302).


Each term can be executed in `STM m` monad:
```hs
execTerm :: (MonadSTM m, MonadCatch (STM m))
         => ExecEnv m
         -> Term t
         -> STM m (ExecValue m t)
```
See [`execTerm`](https://github.com/input-output-hk/io-sim/blob/93858df930c160450e12f34428b61e74813b14ae/io-sim/test/Test/STM.hs#L465).

`execTerm` and `evalTerm` all together give three ways of executing a `Term`:

* via the spec (as implemented by `evalTerm`)
* in `IO` (using `stm` package)
* in `IOSim` (using the built-in `stm` support)

The last two are only possible because `execTerm` is written in terms of
`MonadSTM m` and `MonadCatch (STM m)` are available from `io-classes`.

This gave us confidence that the most tricky operator `orElse` is
implemented correctly.

The [`Test.Control.Monad.STM`] module also implements an `Arbitrary` instance (with a proper
shrinker) of `Terms`s.  The heart of it is the `genTerm` function which
generates arbitrary expressions of a given type.

### Simulated Network Interfaces

Cardano must run on various platforms and support different communication
bearers, e.g. it communicates via TCP/IP between nodes, but it also exposes
an IPC using UNIX sockets.  That would be fine if we'd need to only support
`Linux` and `MacOS`.  However, many end users who are running a full wallet
are using `Windows` machines, on which UNIX sockets are not well supported.
`Windows` has its own [named pipes] API.  Although similar, their interface is
a bit different than the familiar Berkeley socket interface.  However, it's
possible to embrace both using the following `Snocket` API:

```hs
-- | Abstract communication interface that can be used by 'Socket' or a named pipe.
-- Snockets are polymorphic over monad, which is useful for testing and/or
-- simulations.
--
data Snocket m fd addr = Snocket {
    getLocalAddr  :: fd -> m addr
  , getRemoteAddr :: fd -> m addr

  , addrFamily    :: addr -> AddressFamily addr

  -- | Open a file descriptor  (socket / named pipe).  For named pipes, it is
  -- using 'CreateNamedPipe' syscall, for Berkeley sockets 'socket' is used.
  --
  , open          :: AddressFamily addr -> m fd

    -- | A way to create 'fd' to pass to 'connect'.  For named pipes, it will
    -- use 'CreateFile' syscall.  For Berkeley sockets it is the same as 'open'.
    --
    -- For named pipes, we need full 'addr' rather than just address family as
    -- it is for sockets.
    --
  , openToConnect :: addr -> m fd

    -- | `connect` is only needed for Berkeley sockets, for named pipes this is
    -- no-op.
    --
  , connect       :: fd -> addr -> m ()
  , bind          :: fd -> addr -> m ()
  , listen        :: fd -> m ()

  , accept        :: fd -> m (Accept m fd addr)

  , close         :: fd -> m ()
  }
```

`Snocket` is parametrised by the monad in which it runs, file descriptor type,
and address type.  The difference between Berkeley sockets and named pipes lies
in the `accept` call (which will not cover in this blog, see [`Accept`]).

There are four ways to construct a `Snocket`:

* [`socketSnocket`] - which is using the Berkeley `socket` API (and thus
  supports both `AF_INET`, `AF_INET6` and `AF_UNIX` families);
* [`localSnocket`] - a local socket: using `AF_UNIX` on systems on which
  Berkeley sockets are available or named pipes on Windows;
* [`withSnocket`] - an implementation of `Snocket` dedicated for `IOSim`
  simulations.

`Snocket` interface allows us to parametrise the Cardano diffusion
layer ([`Ouroboros.Network.Diffusion.P2P.runM`][cardano-diffusion]) and either
run in `IO` with `Snocket`s provided by the system calls and at the same time
build `IOSim` simulations which run multiple Cardano diffusion layers.  The
`withSnocket` API allows to embed network errors, simulating network delays, TCP
simultaneous open (which is not infrequent in the Cardano ecosystem).  For
an example we test that when `accept` call errors the application can recover
(this turned out to be an issue once we discovered a bug in `MacOS` kernel, see
[ref][macos-bug]).  `IOSim` allowed us to simulate the buggy behaviour of
`MacOS` and verify that our solution indeed fixes the problem.  What is
outstanding for the [`Ouroboros.Network.Diffusion.P2P.runM`][cardano-diffusion]
function is that its entirely described by classes from [`io-classes`] and
[`si-timers`] packages, while it's responsible for:

* maintaining bidirectional connections with remote peers (which turned out to
  be more complex than we initially anticipated, which involves a handful of
  states and transitions);
* running multiplexed protocol applications: this includes making decisions
  like from which peer to download a block based on real-time data and
  executing such a decision via one of the protocols;
* resolving DNS names;
* provide safety guarantees (e.g. if one of the protocol errors the connection
  must be reset);
* keeping remote nodes honest about their timeliness obligations (i.e. enforcing
  network timeouts).

Since the architecture is inherently concurrent: each connection multiplexes up
to four concurrent mini-protocols in both directions (which makes at least
8 threads (even 16 if we count our current implementation of network protocol
pipelining), not counting some other concurrent services.  Being able to
execute the whole diffusion layer in a deterministic simulation environment
which supports race discovery helped us to discover and fix deadlocks and rare
bugs which otherwise would be very difficult to debug in real-system.

### One code base for production & testing

To run the same codebase for production and testing we use one more trick.  We
use the [`contra-tracer`] library.  The current implementation of `Tracer` is
more complex (so that it's zero cost abstraction if tracing is not used), but
for this blog post we can assume that the `Tracer` is defined as:

```haskell
newtype Tracer m a = Tracer { withTracer :: a -> m () }

instance Contravariant (Tracer m) where
    contramap f (Tracer g) = Tracer (g . f)
```

In testing, we can instantiate the tracer with something like
[`Control.Monad.IOSim.traceM`][traceM].  With the help of
[`Control.Monad.IOSim.selectTraceEventsDynamic`][selectTraceEventsDynamic] one
can recover the trace and build a quickcheck property that must be satisfied.
In this way `Tracer` is our eye into the underlying state of the system.

In the production environment, a dedicated logging backend is behind the
`Tracer` abstraction.

The contravariant nature of the `Tracer` is very helpful.  If you are testing
your component you might have access to the logs it's emitting, but in the real
implementation the component might be embedded deep inside your stack and it's
logs might as well be embedded into some larger data structure or even more
importantly with more context.  Contravariant tracer allows you to peal the
onion as you pass the tracer deeper into the stack, adding the extra
information on the way.  Contravariant tracing allows for avoiding low-level
components to have access to high-level tracing information, and make it
possible to decouple and isolate components even if they depend on each other.

## Some IOSim features

### Trace

[`io-sim`] not only allows one to run simulations but also gives access to
detailed traces.  With [Control.Monad.IOSim.runSimTrace][runSimTrace] function
you can get a trace that contains timed execution events including fork
events, blocking / unblocking STM events, synchronous and asynchronous
exceptions (including blocking information of `throwTo`), forking events, delay
& timers events.  The [`SimTrace`] can be pretty-printed with
[Control.Monad.IOSim.ppTrace][ppTrace].  You also have control over the names
of threads and `TVar`s.  And as we mentioned you can extract information logged
by the code in simulation.  From the `SimTrace` you can also extract the result
computed by your simulation with
[Control.Monad.IOSim.traceResult][traceResult].  These three functions are
useful for example to enhance test failure information (e.g. via the well known
[`counterexample`] from the `QuickCheck` library).

You can use [`runSim`] or [`runSimOrThrow`] if you are not interested in the
trace but you just want to get the result.  Note that the `SimTrace` is very
verbose, and thus it might include too much information to analyse simple
problems, but it is indispensable when analysing concurrency bugs.

[`io-sim`] also allows inspecting values committed to `TVar`s in an `STM`
transaction.  In the early days, we relied on tracing, but that can be
rescheduled and thus reorder events.
[Control.Monad.Class.MonadSTM.MonadTraceSTM][MonadTraceSTM] provides the API.
`IOSim` attaches the callbacks to its `TVar`s and executes them whenever an
`STM` transaction is committed.  The callbacks allow one to either log
`Strings` (the same way as the [`say`] from
[Control.Monad.Class.MonadSay][MonadSay] module does), or arbitrary data as
[`traceM`][traceM] does.

[`io-sim`]'s `STM` monad allows to log values with [`traceSTM`].  Since this
function is not polymorphic over monad, it is usage is mostly limited to
debugging stm transactions.

### Laziness

`IOSim`(in both flavours: _IOSim_ and _IOSimPOR_) is executed in lazy `ST`
monad.  This means that the trace is created lazily.  This allows us to
provide a simple `MonadFix` instance, but also allows one to simulate
applications that never exit and then only analyse a finite portion of
a trace.


[Cardano]: https://www.github.com/input-output-hk/cardano-node#readme
[SI]: https://www.wikiwand.com/en/International_System_of_Units
[`Test.Control.Monad.STM`]: https://github.com/input-output-hk/io-sim/blob/master/io-sim/test/Test/Control/Monad/STM.hs
[`async`]: https://hackage.haskell.org/package/async
[`contra-tracer`]:  https://hackage.haskell.org/package/contra-tracer
[`io-classes-mtl`]: https://hackage.haskell.org/package/io-classes-mtl
[`io-classes`]: https://hackage.haskell.org/package/io-classes 
[`io-sim`]: https://hackage.haskell.org/package/io-sim 
[`lifted-async`]: https://hackage.haskell.org/package/lifted-async
[`nothunks`]: https://hackage.haskell.org/package/nothunks
[`registerTimeout`]: https://hackage.haskell.org/package/base-4.17.0.0/docs/GHC-Event.html#v:registerTimeout
[`si-timers`]: https://hackage.haskell.org/package/si-timers 
[`strict-stm`]: https://hackage.haskell.org/package/strict-stm
[`strict-mvar`]: https://hackage.haskell.org/package/strict-mvar
[cardano-diffusion]: https://github.com/input-output-hk/ouroboros-network/blob/db633ec71eff9b2b7797ad0cce0c130771b8cf0c/ouroboros-network/src/Ouroboros/Network/Diffusion/P2P.hs#L531
[io-completion-ports]: https://learn.microsoft.com/en-us/windows/win32/fileio/i-o-completion-ports
[macos-bug]: https://github.com/input-output-hk/ouroboros-network/pull/3388/commits/a8ba8f5224c22795f3ed9d91827ac104005e8c7d
[named pipes]: https://learn.microsoft.com/en-us/windows/win32/ipc/named-pipes
[ouroboros-network]: https://www.github.com/input-output-hk/ouroboros-network#readme
[selectTraceEventsDynamic]: https://github.com/input-output-hk/io-sim/blob/main/io-sim/src/Control/Monad/IOSim.hs#L174
[sim-tcp-open-bug]: https://github.com/input-output-hk/ouroboros-network/pull/4265
[stm-paper]: https://research.microsoft.com/en-us/um/people/simonpj/papers/stm/stm.pdf
[`network-mux`]: https://github.com/input-output-hk/ouroboros-network/tree/master/network-mux

[`Accept`]: https://input-output-hk.github.io/ouroboros-network/ouroboros-network-framework/Ouroboros-Network-Snocket.html#t:Accept
[`socketSnocket`]: https://github.com/input-output-hk/ouroboros-network/blob/db633ec71eff9b2b7797ad0cce0c130771b8cf0c/ouroboros-network-framework/src/Ouroboros/Network/Snocket.hs#L290
[`localSnocket`]: https://github.com/input-output-hk/ouroboros-network/blob/db633ec71eff9b2b7797ad0cce0c130771b8cf0c/ouroboros-network-framework/src/Ouroboros/Network/Snocket.hs#L378
[`withSnocket`]: https://github.com/input-output-hk/ouroboros-network/blob/db633ec71eff9b2b7797ad0cce0c130771b8cf0c/ouroboros-network-framework/src/Simulation/Network/Snocket.hs#L376

[traceM]: https://hackage.haskell.org/package/io-sim-1.0.0.0/docs/Control-Monad-IOSim.html#v:traceM
[`runSim`]: https://hackage.haskell.org/package/io-sim-1.0.0.0/docs/Control-Monad-IOSim.html#v:runSim
[`runSimOrThrow`]:  https://hackage.haskell.org/package/io-sim-1.0.0.0/docs/Control-Monad-IOSim.html#v:runSimOrThrow
[runSimTrace]: https://hackage.haskell.org/package/io-sim-1.0.0.0/docs/Control-Monad-IOSim.html#v:runSimTrace
[`SimTrace`]: https://hackage.haskell.org/package/io-sim-1.0.0.0/docs/Control-Monad-IOSim.html#t:SimTrace
[ppTrace]: https://hackage.haskell.org/package/io-sim-1.0.0.0/docs/Control-Monad-IOSim.html#v:ppTrace
[`traceResult`]: https://hackage.haskell.org/package/io-sim-1.0.0.0/docs/Control-Monad-IOSim.html#v:traceResult
[`counterexample`]: https://hackage.haskell.org/package/QuickCheck-2.14.2/docs/Test-QuickCheck.html#v:counterexample
[`MonadInspectSTM`]: https://hackage.haskell.org/package/io-classes-1.0.0.0/docs/Control-Monad-Class-MonadSTM.html#t:MonadInspectSTM
[MonadTraceSTM]: https://hackage.haskell.org/package/io-classes-1.0.0.0/docs/Control-Monad-Class-MonadSTM.html#t:MonadTraceSTM
[MonadSay]: https://hackage.haskell.org/package/io-classes-1.0.0.0/docs/Control-Monad-Class-MonadSay.html#t:MonadSay
[`say`]: https://hackage.haskell.org/package/io-classes-1.0.0.0/docs/Control-Monad-Class-MonadSay.html#v:say
[`traceSTM`]: https://hackage.haskell.org/package/io-sim-1.0.0.0/docs/Control-Monad-IOSim.html#v:traceSTM
