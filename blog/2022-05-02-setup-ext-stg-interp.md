---
slug: 2022-05-02-setup-ext-stg-interp
title: Setting up Csaba's External STG Interpreter
date: May 2, 2022
authors: [ doyougnu ]
tags: [ghc stg tooling profiling optimization]
---

## Table of Contents
- [Making sense of the project](#orgfeb334e)
- [Building a working external STG interpreter](#org1d461dc)
  - [ghc.nix](#orgb670539)
  - [Building ghc-wpc](#orgbb3f1d5)
  - [Building the stg tooling](#org9ef4bc5)
- [Building the external-stg-interpreter](#org4a2eaf9)
- [Linking the external-stg-interpreter](#org1d34a2e)
- [The whole setup process on a demo](#org2daa4b8)
- [Summary](#org8193a1a)
  - [File Descriptions](#org940ba90)
  - [Step-by-Step guide for running the interpreter on your code](#org8e9f409)

Haskell is a great language camouflaged by lackluster tooling. This situation
has led to well-known problems (who could forget Cabal hell?). A less discussed
problem is what I will call the &ldquo;Black-box syndrome&rdquo;: It is hard to
know *exactly* what the memory representation and runtime performance of my
Haskell programs are[^1]. Now black-box syndrome is not *only* a problem,
it is also one of the nice features in the language since like all good
abstractions it elides things I&rsquo;d rather not care about, at least most of
the time. In other words, I am happy I don&rsquo;t have to do manual memory
manipulation!

However, when I have my optimization hat on, I run face first into black-box syndrome. The crux of the problem is a tension between the need for observation during performance engineering and optimization, and the need to ship fast code. During development we want to be able to open up a system, see exactly how it is working, make tweaks, package it back up and test again. I want to be able to answer questions like &ldquo;Why is my executable this size?&rdquo;, &ldquo;Which code is a hot loop?&rdquo;, or &ldquo;When does my code do direct, known or unknown function calls?&rdquo;.

In order to answer these questions we need the ability to observe *every part of that system as the machine experiences it*, without this ability we have no way to make progress other than test, change some code, compile and test again in an ad-hoc manner. And therein lies the problem, most Haskell tooling is insufficient to provide the observability that we would like, instead the tooling often expects and requires us to make source code changes to our program or even recompile all of our libraries and code for a profiling way. This leads to the idea and *the expectation* in the Haskell community that Haskell programs are hard to optimize because the barrier to entry for optimization has artificially increased.

[Csaba Hruska](https://www.patreon.com/csaba_hruska) has recently been making headway in this area with his work on the [GRIN](https://youtu.be/iXhh0NSR67k) compiler and an external STG interpreter. His STG interpreter (and patched ghc) exactly solve these problems and he has demonstrated dumping the entire call graph of large Haskell projects, filter to hot loops and finding unknown function calls in these graphs. If you haven&rsquo;t seen his [demo](https://www.youtube.com/watch?v=wt6iCgYmVGA&t=2054s) be sure to watch it, it is well worth your time.

This post is the first in a new blog series. In this blog series we&rsquo;re going to kick the tires on the external STG interpreter see what it can do, and what we can uncover in some popular libraries by using it. In particular, I&rsquo;m interested in running it on projects I&rsquo;ve previously optimized&#x2014;such as ghc itself, containers, unordered-containers&#x2014;using the standard methods: ticky-ticky profiling, prof, flamegraphs, heap profiling, ghc-debug, cachegrind etc. This post, however, will be focused on setting up the patched ghc and interpreter on a NixOS system. My goals are threefold:

1.  Give an overview of the project and project layout to lower barrier to entry for the system.
2.  Give step by step instructions on setting up the interpreter on a nix-based system and provide a forked github repo for nix users. This should allow nix users to just `git clone foo` and `nix-build` (spoiler: it won&rsquo;t be that easy but still not hard.)
3.  Popularize Csaba&rsquo;s project! It is a refreshing take on Haskell optimization and compilation.


<a id="orgfeb334e"></a>

# Making sense of the project

The external STG interpreter is part of the [GRIN compiler](https://github.com/grin-compiler) project. We are not doing anything with the GRIN compiler (yet!) and so we are only interested in [The GHC whole compiler project](https://github.com/grin-compiler/ghc-whole-program-compiler-project). The whole-compiler-project has several sub-projects that we&rsquo;ll be building and using directly:

-   [external-stg](https://github.com/grin-compiler/ghc-whole-program-compiler-project/tree/master/external-stg): This subproject provides utilites we&rsquo;ll be using, in particular `mkfullpak`
-   [external-stg-interpreter](https://github.com/grin-compiler/ghc-whole-program-compiler-project/tree/master/external-stg-interpreter): This is the actual STG interpreter. The good news is that this is independent of the rest of the project and can be built just like a regular Haskell executable
-   [ghc-wpc](https://github.com/grin-compiler/ghc-wpc/tree/b51ab235f5c07caa5eb3dd3b40487f67f50fb838): This is a fork of `ghc-8.10.x` (I&rsquo;m not sure exactly which version it forks to be honest) which we must build in order to use the external STG interpreter. Ghc-wpc serves as a frontend for the external-stg-interpreter.


<a id="org1d461dc"></a>

# Building a working external STG interpreter

The external STG interpreter can be built like any regular haskell executable. But in order to use the interpreter we have to build `ghc-wpc`. `ghc-wpc` is necessary because it serves as a frontend for the STG interpreter. It compiles a Haskell program like normal and then dumps an enriched STG IR to file. This file is then run through a utility `gen-exe` (gen-exe is an executable built in the [external-stg-compiler](https://github.com/grin-compiler/ghc-whole-program-compiler-project/tree/master/external-stg-compiler) sub-project) which picks up the compilation pipeline from the STG IR and creates an executable like we would expect from a normal compilation pipeline.

The major difference between this process and the usual compiler pipeline is that `ghc-wpc` leaves enough compiler information on disk for the rest of the tooling to consume, namely, in files with a `*.o_stgbin` (this is STG IR generated at compile time), and `*.o_stgapp` (project linker and dependency information) extension. Thus, once we build this custom ghc version we can use it to build the source code we wish to analyze and begin our optimization work.

For the rest of this tutorial I&rsquo;ll be referencing my [fork](https://github.com/doyougnu/ghc-whole-program-compiler-project) of the `ghc-whole-compiler-project` that includes everything you need if you want to follow along, including `.nix` files for creating a `nix-shell` which will prepare a suitable environment to run the entire toolchain.


<a id="orgb670539"></a>

## ghc.nix

The usual way to build ghc using a nix based system is with the [ghc.nix](https://github.com/alpmestan/ghc.nix) project. Ghc.nix provides a `default.nix` with a suitable environment to run hadrian and build ghc. For `ghc-wpc` we&rsquo;ll need some special packages, and we need our boot compiler to be *exactly* `ghc-8.3.3`. The custom `ghc.nix` file is included in my fork, I&rsquo;ve taken the liberty to pin the nixpkgs to the right version for `ghc-8.3.3`. So let&rsquo;s begin:

Clone the forked repo:

```bash
$ git clone https://github.com/doyougnu/ghc-whole-program-compiler-project.git

$ cd ghc-whole-program-compiler-project

$ tree -L 1
.
├── dist-newstyle
├── external-stg
├── external-stg-compiler
├── external-stg-interpreter
├── ghc.nix.wpc
├── ghc-wpc
├── lambda
├── mod-pak
├── README.md
├── shell.nix
├── stack.yaml
└── stack.yaml.lock
```

You&rsquo;ll find the patched `ghc.nix` included (`ghc.nix.wpc`) and a `shell.nix` for a `nix-shell`. The `shell.nix` file simply references `ghc.nix.wpc/default.nix` with the appropriate options:

```nix
$ cat shell.nix
import (./ghc.nix.wpc/default.nix) {
useClang = true;
withHadrianDeps = true;
withIde   = false;
withLlvm  = true;
}
```


<a id="orgbb3f1d5"></a>

## Building ghc-wpc

Now we can enter a nix-shell and build `ghc-wpc`:

```bash
$ pwd
/home/doyougnu/programming/haskell/ghc-whole-program-compiler-project

$ nix-shell shell.nix  # or just nix-shell
trace: checking if /home/doyougnu/programming/haskell/ghc-whole-program-compiler-project/hadrian/hadrian.cabal is present:  no
Recommended ./configure arguments (found in $CONFIGURE_ARGS:
or use the configure_ghc command):

  --with-gmp-includes=/nix/store/sznfxigwvrvn6ar3nz3f0652zsld9xqj-gmp-6.2.0-dev/include
  --with-gmp-libraries=/nix/store/447im4mh8gmw85dkrvz3facg1jsbn6c7-gmp-6.2.0/lib
  --with-curses-includes=/nix/store/84g84bg47xxg01ba3nv0h418v5v3969n-ncurses-6.1-20190112-dev/include
  --with-curses-libraries=/nix/store/xhhkr936b9q5sz88jp4l29wljbbcg39k-ncurses-6.1-20190112/lib
  --with-libnuma-includes=/nix/store/bfrcskjspk9a179xqqf1q9xqafq5s8d2-numactl-2.0.13/include
  --with-libnuma-libraries=/nix/store/bfrcskjspk9a179xqqf1q9xqafq5s8d2-numactl-2.0.13/lib
  --with-libdw-includes=/nix/store/sv6f05ngaarba50ybr6fdfc7cciv6nbv-elfutils-0.176/include
  --with-libdw-libraries=/nix/store/sv6f05ngaarba50ybr6fdfc7cciv6nbv-elfutils-0.176/lib
  --enable-dwarf-unwind

[nix-shell:~/programming/haskell/ghc-whole-program-compiler-project]$
```

Now we need to `cd` into `ghc-wpc` and tweak the hadrian build.

**MAJOR CONSTRAINT: You must build ghc-wpc with hadrian/build-stack**, if you build in any other way you&rsquo;ll run into shared object errors, see this [ticket](https://github.com/grin-compiler/ghc-whole-program-compiler-project/issues/4) for details.

So in order to build `ghc-wpc` with stack we&rsquo;ll have to tweak the `stack.yaml` file. **You must do this since it is not included in the fork**:

Quick side note: To make the formatting nicer I truncate
`nix-shell:~/foo/bar/baz/ghc-whole-program-compiler-project` to just `...`, so
`nix-shell:.../ghc-wpc` is equivalent to
`~/path/to/ghc-whole-compiler-project/ghc-wpc`.

```bash
[nix-shell:...]$ cd ghc-wpc/hadrian/

[nix-shell:.../ghc-wpc/hadrian]$ cat stack.yaml
resolver: lts-15.5

packages:
- '.'
- 'GHC-Cabal'

system-ghc: true

nix:
   enable: true
   shell-file: ../../shell.nix
```

The changes are: (1) tell `stack` we are using `nix`, and (2) reference the `shell.nix` file which points to `ghc.wpc.nix` at the root of the project, i.e., `ghc-whole-program-compiler-project/shell.nix`.

Now we should be able to begin our build, return to the root of `ghc-wpc` and run the following:

```bash
[nix-shell:.../ghc-wpc/hadrian]$ cd ..

[nix-shell:.../ghc-wpc]$ ./boot && ./configure

[nix-shell:.../ghc-wpc]$ hadrian/build-stack -j
```

and go get some coffee since this will take some time. Once it finishes you should have the `ghc-wpc` binary in `_build/stage1/bin`

```bash
[nix-shell:.../ghc-wpc]$ ls -l _build/stage1/bin/
total 8592
-rwxr-xr-x 1 doyougnu users 1843752 Apr 29 23:01 ghc
-rw-r--r-- 1 doyougnu users   11082 Apr 29 23:01 ghc.dyn_o_ghc_stgapp
-rwxr-xr-x 1 doyougnu users  660128 Apr 29 22:50 ghc-pkg
-rw-r--r-- 1 doyougnu users    9977 Apr 29 22:50 ghc-pkg.dyn_o_ghc_stgapp
-rwxr-xr-x 1 doyougnu users 4624680 Apr 29 23:01 haddock
-rw-r--r-- 1 doyougnu users   16883 Apr 29 23:01 haddock.dyn_o_ghc_stgapp
-rwxr-xr-x 1 doyougnu users   49344 Apr 29 22:25 hp2ps
-rw-r--r-- 1 doyougnu users    2504 Apr 29 22:25 hp2ps.dyn_o_ghc_stgapp
-rwxr-xr-x 1 doyougnu users  716440 Apr 29 22:35 hpc
-rw-r--r-- 1 doyougnu users    9959 Apr 29 22:35 hpc.dyn_o_ghc_stgapp
-rwxr-xr-x 1 doyougnu users  738544 Apr 29 22:35 hsc2hs
-rw-r--r-- 1 doyougnu users   10264 Apr 29 22:35 hsc2hs.dyn_o_ghc_stgapp
-rwxr-xr-x 1 doyougnu users   58384 Apr 29 22:34 runghc
-rw-r--r-- 1 doyougnu users    8864 Apr 29 22:34 runghc.dyn_o_ghc_stgapp
```

Notice that this build dumped `*.<way>_o_ghc_stgapp` files!


<a id="org9ef4bc5"></a>

## Building the stg tooling

Now that we have a working `ghc-wpc` we need to build the rest of the project by pointing `stack` to the `ghc-wpc` binary in `ghc-wpc/_build/stage1/bin`. That is, we must change the `ghc-whole-program-compiler-project/stack.yaml` file:

```bash
[nix-shell:~/programming/haskell/ghc-whole-program-compiler-project]$ cat stack.yaml
resolver: lts-16.13

allow-newer: true

packages:
  - 'external-stg-compiler'
  - 'external-stg'

ghc-options:
  "$everything": -fno-stgbin -fno-stgapp -optcxx-std=c++17

extra-deps:
  - async-pool-0.9.1@sha256:4015140f896c3f1652b06a679b0ade2717d05557970c283ea2c372a71be2a6a1,1605
  - souffle-haskell-1.1.0
  - zip-1.7.0


# use custom ext-stg whole program compiler GHC
compiler:     ghc-8.11.0
skip-ghc-check: true

nix:
  enable: false


# use local GHC (for development)
system-ghc: true
extra-path:
  - /home/doyougnu/programming/haskell/ghc-whole-program-compiler-project/ghc-wpc/_build/stage1/bin

# DEBUG INFO
#dump-logs: all
#build:
#  keep-tmp-files: true
#  cabal-verbose: true
```

The changes are: (1) set `compiler: ghc-8.11.0` (the `ghc-wpc` fork), (2) set `skip-ghc-check: true` so that stack doesn&rsquo;t complain about the ghc version, (3) set `nix.enable: false`, confusingly if you leave this as true then stack will try to use `nixpkgs` to get a ghc binary, but we want it to use our local binary so we disable this even though we&rsquo;ll still be in our original nix-shell (4) set `system-path: true` to tell stack we will be using a ghc we have on our system, and finally (5) set `extra-path: <path-to-ghc-wpc-binary>`.

Now we can run stack and install the stg tooling:

```bash
[nix-shell:...]$ stack --stack-root `pwd`/.stack-root install
Trouble loading CompilerPaths cache: UnliftIO.Exception.throwString called with:

Compiler file metadata mismatch, ignoring cache
Called from:
  throwString (src/Stack/Storage/User.hs:277:8 in stack-2.7.5-9Yv1tjrmAU3JiZWCo86ldN:Stack.Storage.User)

WARNING: Ignoring tagged's bounds on template-haskell (>=2.8 && <2.17); using template-haskell-2.17.0.0.
Reason: allow-newer enabled.
WARNING: Ignoring aeson's bounds on template-haskell (>=2.9.0.0 && <2.17); using template-haskell-2.17.0.0.
Reason: allow-newer enabled.
WARNING: Ignoring th-abstraction's bounds on template-haskell (>=2.5 && <2.17); using template-haskell-2.17.0.0.
Reason: allow-newer enabled.
WARNING: Ignoring unliftio-core's bounds on base (>=4.5 && <4.14); using base-4.14.0.0.
Reason: allow-newer enabled.
WARNING: Ignoring souffle-haskell's bounds on megaparsec (>=7.0.5 && <8); using megaparsec-8.0.0.
stack --stack-root `pwd`/.stack-root install
... # bunch of output
...
...
Copied executables to /home/doyougnu/.local/bin:
- dce-fullpak
- ext-stg
- fullpak
- gen-exe
- gen-exe2
- gen-obj
- gen-obj2
- mkfullpak
- show-ghc-stg

Warning: Installation path /home/doyougnu/.local/bin not found on the PATH environment variable.
```

You can add `~/.local/bin` to your `PATH` if you want, I&rsquo;ll just be directly referencing these binaries as we go.


<a id="org4a2eaf9"></a>

# Building the external-stg-interpreter

We are almost all done, all that is left is to build the external-stg-interpreter and run a small script that links everything together into a shared object for the interpreter. So:

```bash
[nix-shell:...]$ cd external-stg-interpreter/

[nix-shell:.../external-stg-interpreter]$ stack install
...  # bunch of output
...
Copied executables to /home/doyougnu/.local/bin:
- ext-stg
- ext-stg-interpreter
- fullpak
- mkfullpak

Warning: Installation path /home/doyougnu/.local/bin not found on the PATH environment variable.
```

Now we have our `ext-stg-interpreter` built! There are a few caveats I want to point out here. I&rsquo;ve modified `ghc-whole-program-compiler-project/external-stg-interpreter/stack.yaml` to load the right packages and use nix:

```bash
[nix-shell:.../external-stg-interpreter]$ cat stack.yaml
resolver: lts-16.13

packages:
  - '.'
  - 'external-stg'

extra-deps:
  - souffle-haskell-2.1.0
  - primitive-0.7.1.0
  - zip-1.7.0

nix:
  enable: true
  packages: [ zlib, libffi, pkg-config, bzip2 ]
```

Notice the `nix:` block. We could have just as easily built this using `nix` directly or using our `shell.nix` file.


<a id="org1d34a2e"></a>

# Linking the external-stg-interpreter

The only task left is to link into a shared object library called
`libHSbase-4.14.0.0.cbits.so`. To do that we need to use the script called, `c`,
in `ghc-whole-program-compiler-project/external-stg-interpreter/data`. This
script is a bit of a hack, it generates the shared object file so that we can link the symbols requested by the C
FFI in `base`, but it populates those functions with our replacements, which do absolutely nothing. For example, we supply a fake garbage collect:
```c
// in .../external-stg-interpreter/data/cbits.so-script/c-src/fake_rts.c
...
void performGC(void) {
}

void performMajorGC(void) {
}
...
```

This works because we won't be using the runtime system at all, we'll be using
the external STG interpreter instead, however we still need to provide these
symbols in order to link. ****MAJOR NOTE: this file must be next to any
\*.fullpak file you&rsquo;ll be running the interpreter on**** or else
you&rsquo;ll get an undefined symbol error during linking, for example:

```bash
[nix-shell:.../external-stg-interpreter/data]$ ls
cbits.so-script  ghc-rts-base.fullpak  minigame-strict.fullpak

### notice no .so file
[nix-shell:.../external-stg-interpreter/data]$ ~/.local/bin/ext-stg-interpreter ghc-rts-base.fullpak
ext-stg-interpreter: user error (dlopen: ./libHSbase-4.14.0.0.cbits.so: cannot open shared object file: No such file or directory)

## we error'd out because it was missing, also
## if you get this error then you have an old cbits.so file and need to rerun the c script
[nix-shell:.../external-stg-interpreter/data]$ ~/.local/bin/ext-stg-interpreter ghc-rts-base.fullpak
ext-stg-interpreter: user error (dlopen: ./libHSbase-4.14.0.0.cbits.so: undefined symbol: getProcessElapsedTime)
```

To link the interpreter we need to run `c` in the `data/cbits.so-script` sub-folder:

```bash
[nix-shell:.../external-stg-interpreter]$ cd data/cbits.so-script/

[nix-shell:.../external-stg-interpreter/data/cbits.so-script]$ ls
ar  c  cbits-rts.dyn_o  c-src  libHSbase-4.14.0.0.cbits.so  stub-base.dyn_o

[nix-shell:.../external-stg-interpreter/data/cbits.so-script]$ ./c
++ ls ar/libHSbase-4.14.0.0-ghc8.11.0.20210220.dyn_o_cbits.a ar/libHSbindings-GLFW-3.3.2.0-Jg9TvsfYUZwD0ViIP0H2Tz-ghc8.11.0.20210306.dyn_o_cbits.a ar/libHSbytestring-0.10.9.0-ghc8.11.0.20210306.dyn_o_cbits.a ar/libHScriterion-measurement-0.1.2.0-73BCI2Fnk7qE8QjjTa1xNa-ghc8.11.0.20210324.dyn_o_cbits.a ar/libHSghc-8.11.0.20210306-ghc8.11.0.20210306.dyn_o_cbits.a ar/libHSGLUT-2.7.0.15-1pzTWDEZBcYHcS36qZ2lpp-ghc8.11.0.20201112.dyn_o_cbits.a ar/libHSGLUT-2.7.0.15-1pzTWDEZBcYHcS36qZ2lpp-ghc8.11.0.20210324.dyn_o_stubs.a ar/libHShashable-1.3.0.0-Kn7aNSFvzgo2qY16wYzuCX-ghc8.11.0.20210306.dyn_o_cbits.a ar/libHSinteger-gmp-1.0.3.0-ghc8.11.0.20210220.dyn_o_cbits.a ar/libHSlambdacube-quake3-engine-0.1.0.0-7CKLP3Rqgq0PR81lhlwlR-ghc8.11.0.20210306.dyn_o_cbits.a ar/libHSmersenne-random-pure64-0.2.2.0-ExYg8DmthtrLG9JevQbt2m-ghc8.11.0.20210306.dyn_o_cbits.a ar/libHSOpenGLRaw-3.3.4.0-5vXBlmbOM3AIT7GRYfpE3o-ghc8.11.0.20201112.dyn_o_cbits.a ar/libHSprimitive-0.7.0.1-2k3g9qX0zz16vEv34R307m-ghc8.11.0.20210306.dyn_o_cbits.a ar/libHSprocess-1.6.8.2-ghc8.11.0.20210220.dyn_o_cbits.a ar/libHStext-1.2.4.0-ghc8.11.0.20210220.dyn_o_cbits.a ar/libHSunix-2.7.2.2-ghc8.11.0.20210220.dyn_o_cbits.a ar/libHSunix-2.7.2.2-ghc8.11.0.20210220.dyn_o_stubs.a ar/libHSzlib-0.6.2.1-1I6DmfbLEyTBgDZI7SbZfW-ghc8.11.0.20210306.dyn_o_stubs.a
++ ls stub-base.dyn_o/Blank_stub.dyn_o stub-base.dyn_o/ClockGetTime_stub.dyn_o stub-base.dyn_o/Internals_stub.dyn_o stub-base.dyn_o/RUsage_stub.dyn_o
++ ls cbits-rts.dyn_o/StgPrimFloat.dyn_o cbits-rts.dyn_o/TTY.dyn_o
++ ls c-src/fake_rts.c c-src/hack.c c-src/hschooks.c
+ gcc -o libHSbase-4.14.0.0.cbits.so -shared -Wl,--whole-archive ar/libHSbase-4.14.0.0-ghc8.11.0.20210220.dyn_o_cbits.a ar/libHSbindings-GLFW-3.3.2.0-Jg9TvsfYUZwD0ViIP0H2Tz-ghc8.11.0.20210306.dyn_o_cbits.a ar/libHSbytestring-0.10.9.0-ghc8.11.0.20210306.dyn_o_cbits.a ar/libHScriterion-measurement-0.1.2.0-73BCI2Fnk7qE8QjjTa1xNa-ghc8.11.0.20210324.dyn_o_cbits.a ar/libHSghc-8.11.0.20210306-ghc8.11.0.20210306.dyn_o_cbits.a ar/libHSGLUT-2.7.0.15-1pzTWDEZBcYHcS36qZ2lpp-ghc8.11.0.20201112.dyn_o_cbits.a ar/libHSGLUT-2.7.0.15-1pzTWDEZBcYHcS36qZ2lpp-ghc8.11.0.20210324.dyn_o_stubs.a ar/libHShashable-1.3.0.0-Kn7aNSFvzgo2qY16wYzuCX-ghc8.11.0.20210306.dyn_o_cbits.a ar/libHSinteger-gmp-1.0.3.0-ghc8.11.0.20210220.dyn_o_cbits.a ar/libHSlambdacube-quake3-engine-0.1.0.0-7CKLP3Rqgq0PR81lhlwlR-ghc8.11.0.20210306.dyn_o_cbits.a ar/libHSmersenne-random-pure64-0.2.2.0-ExYg8DmthtrLG9JevQbt2m-ghc8.11.0.20210306.dyn_o_cbits.a ar/libHSOpenGLRaw-3.3.4.0-5vXBlmbOM3AIT7GRYfpE3o-ghc8.11.0.20201112.dyn_o_cbits.a ar/libHSprimitive-0.7.0.1-2k3g9qX0zz16vEv34R307m-ghc8.11.0.20210306.dyn_o_cbits.a ar/libHSprocess-1.6.8.2-ghc8.11.0.20210220.dyn_o_cbits.a ar/libHStext-1.2.4.0-ghc8.11.0.20210220.dyn_o_cbits.a ar/libHSunix-2.7.2.2-ghc8.11.0.20210220.dyn_o_cbits.a ar/libHSunix-2.7.2.2-ghc8.11.0.20210220.dyn_o_stubs.a ar/libHSzlib-0.6.2.1-1I6DmfbLEyTBgDZI7SbZfW-ghc8.11.0.20210306.dyn_o_stubs.a -Wl,--no-whole-archive stub-base.dyn_o/Blank_stub.dyn_o stub-base.dyn_o/ClockGetTime_stub.dyn_o stub-base.dyn_o/Internals_stub.dyn_o stub-base.dyn_o/RUsage_stub.dyn_o cbits-rts.dyn_o/StgPrimFloat.dyn_o cbits-rts.dyn_o/TTY.dyn_o -fPIC c-src/fake_rts.c c-src/hack.c c-src/hschooks.c -lm -lgmp -ltinfo -lGL -lX11 -lXi -lXrandr -lXxf86vm -lXcursor -lXinerama -lpthread
```

This will produce `libHSbase-4.14.0.0.cbits.so` in the immediate directory:

```bash
[nix-shell:.../external-stg-interpreter/data/cbits.so-script]$ ls -l
total 984
drwxr-xr-x 2 doyougnu users   4096 Apr 27 14:10 ar
-rwxr-xr-x 1 doyougnu users    300 Apr 27 14:10 c
drwxr-xr-x 2 doyougnu users   4096 Apr 27 14:10 cbits-rts.dyn_o
drwxr-xr-x 2 doyougnu users   4096 Apr 27 14:10 c-src
-rwxr-xr-x 1 doyougnu users 986008 Apr 30 11:50 libHSbase-4.14.0.0.cbits.so    ## <----- new
drwxr-xr-x 2 doyougnu users   4096 Apr 27 14:10 stub-base.dyn_o
```

Now we can test our interpreter by running it on the `*.fullpak` files in `external-stg-interpreter/data`:

```bash
[nix-shell:.../external-stg-interpreter/data/cbits.so-script]$ cd ..

[nix-shell:.../external-stg-interpreter/data]$ ls
cbits.so-script  ghc-rts-base-call-graph-summary  ghc-rts-base-call-graph.tsv  ghc-rts-base.fullpak  libHSbase-4.14.0.0.cbits.so  minigame-strict.fullpak

## remove the old .so file
[nix-shell:.../external-stg-interpreter/data]$ rm libHSbase-4.14.0.0.cbits.so

## soft-link to the one we just built
[nix-shell:.../external-stg-interpreter/data]$ ln -s cbits.so-script/libHSbase-4.14.0.0.cbits.so libHSbase-4.14.0.0.cbits.so

[nix-shell:.../external-stg-interpreter/data]$ ls -l
total 79220
drwxr-xr-x 6 doyougnu users     4096 Apr 30 11:50 cbits.so-script
-rw-r--r-- 1 doyougnu users       48 Apr 30 11:47 ghc-rts-base-call-graph-summary
-rw-r--r-- 1 doyougnu users    28238 Apr 30 11:47 ghc-rts-base-call-graph.tsv
-rw-r--r-- 1 doyougnu users 22450708 Apr 27 14:10 ghc-rts-base.fullpak
lrwxrwxrwx 1 doyougnu users       43 Apr 30 11:55 libHSbase-4.14.0.0.cbits.so -> cbits.so-script/libHSbase-4.14.0.0.cbits.so  ### <---- new
-rw-r--r-- 1 doyougnu users 58630129 Apr 27 14:10 minigame-strict.fullpak

[nix-shell:.../external-stg-interpreter/data]$ ~/.local/bin/ext-stg-interpreter ghc-rts-base.fullpak
hello
hello
ssHeapStartAddress: 53522
ssTotalLNECount: 69
ssClosureCallCounter: 360
executed closure id count: 114
call graph size: 150

[nix-shell:.../external-stg-interpreter/data]$ ls -l
total 79220
drwxr-xr-x 6 doyougnu users     4096 Apr 30 11:50 cbits.so-script
-rw-r--r-- 1 doyougnu users       48 Apr 30 11:56 ghc-rts-base-call-graph-summary    ### <---- interpreter output
-rw-r--r-- 1 doyougnu users    28238 Apr 30 11:56 ghc-rts-base-call-graph.tsv        ### <---- interpreter output
-rw-r--r-- 1 doyougnu users 22450708 Apr 27 14:10 ghc-rts-base.fullpak
lrwxrwxrwx 1 doyougnu users       43 Apr 30 11:55 libHSbase-4.14.0.0.cbits.so -> cbits.so-script/libHSbase-4.14.0.0.cbits.so
-rw-r--r-- 1 doyougnu users 58630129 Apr 27 14:10 minigame-strict.fullpak
```

And it works, we have two new files, `<foo>-call-graph-summary` and `<foo>-call-graph.tsv` which we can analyze to inspect the behavior of our program (more on this later).


<a id="org2daa4b8"></a>

# The whole setup process on a demo

That was a rather involved example, to make clear the dependencies and steps required to run this on your own code the rest of this tutorial will run the interpreter on two of Csaba&rsquo;s demo&rsquo;s from his skillshare talk. First let&rsquo;s grab the code:

```bash
$ pwd
/home/doyougnu/programming/haskell

$ git clone https://github.com/grin-compiler/ext-stg-interpreter-presentation-demos.git

$ ls
ext-stg-interpreter-presentation-demos ghc-whole-program-compiler-project ..
```

Now we&rsquo;ll run the first demo which is a simply fold over a list:

```bash
$ nix-shell ghc-whole-program-compiler-project/shell.nix
trace: checking if /home/doyougnu/programming/haskell/hadrian/hadrian.cabal is present:  no
Recommended ./configure arguments (found in $CONFIGURE_ARGS:
or use the configure_ghc command):

  --with-gmp-includes=/nix/store/sznfxigwvrvn6ar3nz3f0652zsld9xqj-gmp-6.2.0-dev/include
  --with-gmp-libraries=/nix/store/447im4mh8gmw85dkrvz3facg1jsbn6c7-gmp-6.2.0/lib
  --with-curses-includes=/nix/store/84g84bg47xxg01ba3nv0h418v5v3969n-ncurses-6.1-20190112-dev/include
  --with-curses-libraries=/nix/store/xhhkr936b9q5sz88jp4l29wljbbcg39k-ncurses-6.1-20190112/lib
  --with-libnuma-includes=/nix/store/bfrcskjspk9a179xqqf1q9xqafq5s8d2-numactl-2.0.13/include
  --with-libnuma-libraries=/nix/store/bfrcskjspk9a179xqqf1q9xqafq5s8d2-numactl-2.0.13/lib
  --with-libdw-includes=/nix/store/sv6f05ngaarba50ybr6fdfc7cciv6nbv-elfutils-0.176/include
  --with-libdw-libraries=/nix/store/sv6f05ngaarba50ybr6fdfc7cciv6nbv-elfutils-0.176/lib
  --enable-dwarf-unwind

[nix-shell:~/programming/haskell]$ cd ext-stg-interpreter-presentation-demos/demo-01-tsumupto/

[nix-shell:~/programming/haskell/ext-stg-interpreter-presentation-demos/demo-01-tsumupto]$ ../../ghc-whole-program-compiler-project/ghc-wpc/_build/stage1/bin/ghc -O2 tsumupto.hs
[1 of 1] Compiling Main             ( tsumupto.hs, tsumupto.o )
Linking tsumupto ...
$ cd ext-stg-interpreter-presentation-demos/demo-01-tsumupto

$ ls
tsumupto  tsumupto.hi  tsumupto.hs  tsumupto.o  tsumupto.o_ghc_stgapp  tsumupto.o_modpak
```

Note, that we have two new files: `*.o_ghc_stgapp` and `.o_modpak` as a result of building with `ghc-wpc`. If you try to run this from outside the nix-shell you&rsquo;ll get an error about missing `mkmodpak`:

```bash
$ ../../ghc-whole-program-compiler-project/ghc-wpc/_build/stage1/bin/ghc -O2 tsumupto.hs
[1 of 1] Compiling Main             ( tsumupto.hs, tsumupto.o )
ghc: could not execute: mkmodpak
```

Now that we have those files we can run the interpreter, but first though we need to make a `*.fullpak` file from the `*.o_ghc_stgapp` file and create a symbolic link to `libHSbase-4.14.0.0.cbits.so`:

```bash
## make the fullpack file
$ ~/.local/bin/mkfullpak tsumupto.o_ghc_stgapp
all modules: 259
app modules: 113
app dependencies:
... # bunch of output
...
main                                                         Main
creating tsumupto.fullpak

## create the link to the shared object file
$ ln -s ../../ghc-whole-program-compiler-project/external-stg-interpreter/data/cbits.so-script/libHSbase-4.14.0.0.cbits.so libHSbase-4.14.0.0.cbits.so

## the final directory should look like this
$ ls
libHSbase-4.14.0.0.cbits.so  tsumupto  tsumupto.fullpak  tsumupto.hi  tsumupto.hs  tsumupto.o  tsumupto.o_ghc_stgapp  tsumupto.o_modpak
```

And now we can run the interpreter:

```bash
$ ~/.local/bin/ext-stg-interpreter tsumupto.fullpak
50005000
ssHeapStartAddress: 44082
ssTotalLNECount: 43
ssClosureCallCounter: 30275
executed closure id count: 112
call graph size: 146
```

The first line is the output of the program and the rest are diagnostics that the interpreter outputs. More importantly we should have a tab-separated csv file and call graph file in our local directory after running the interpreter:

```bash
$ ls -l
total 23876
lrwxrwxrwx 1 doyougnu users      114 Apr 30 12:21 libHSbase-4.14.0.0.cbits.so -> ../../ghc-whole-program-compiler-project/external-stg-interpreter/data/cbits.so-script/libHSbase-4.14.0.0.cbits.so
-rwxr-xr-x 1 doyougnu users  9442648 Apr 30 12:12 tsumupto
-rw-r--r-- 1 doyougnu users       53 Apr 30 12:23 tsumupto-call-graph-summary   ### <---- interpreter output
-rw-r--r-- 1 doyougnu users    27490 Apr 30 12:23 tsumupto-call-graph.tsv       ### <---- interpreter output
-rw------- 1 doyougnu users 14922366 Apr 30 12:19 tsumupto.fullpak
-rw-r--r-- 1 doyougnu users     1769 Apr 30 12:12 tsumupto.hi
-rw-r--r-- 1 doyougnu users      207 Apr 28 22:56 tsumupto.hs
-rw-r--r-- 1 doyougnu users     4488 Apr 30 12:12 tsumupto.o
-rw-r--r-- 1 doyougnu users     8817 Apr 30 12:12 tsumupto.o_ghc_stgapp
-rw------- 1 doyougnu users     9803 Apr 30 12:12 tsumupto.o_modpak
```

Which can be loaded into `gephi` for closer inspection of the call graph of our program. Be sure to watch the rest of the demo in Csaba&rsquo;s talk for this part! For now we&rsquo;ll be going over using `gephi` and these files in our next blog post in this series, stay tuned!


<a id="org8193a1a"></a>

# Summary


<a id="org940ba90"></a>

## File Descriptions

-   `foo.modpak`: A zip file which contains the Core, STG, CMM, source code, and assembly for the module `foo`
-   `foo.fullpak`: A zip file which contains the same information as `modpack` but for every module of the program rather than just module `foo`.
-   `foo.o_ghc_stgapp`: a yaml like file that contains:
    -   the module&rsquo;s dependencies including package dependencies
    -   a bunch of file paths for shared objects of the libraries
    -   the flags the module was built with
-   `libHSbase-4.14.0.0.cbits.so`: shared object file created by `ext-stg-interpreter/data/cbits.so-script.c`. Required to be in the same directory as `ext-stg-interpreter` will be invoked.


<a id="org8e9f409"></a>

## Step-by-Step guide for running the interpreter on your code

1.  Build your project with `ghc-wpc/_build/stage1/bin` by directly invoking that `ghc` (as I did in the demo-01 project) or by pointing stack to it with `system-ghc` and `extra-path` in `stack.yaml`, or by passing `-w <path-to-ghc-wpc-binary` with cabal.
2.  Generate the `foo.fullpak` file with `mkfullpak foo.o_ghc_stgapp`
3.  Soft-link to `libHSbase-4.14.0.0.cbits.so` in the directory you will run the interpreter in. This file must be present when you run the interpreter!
4.  Now run the interpreter on `project.fullpak`
5.  Analyze `foo-call-graph-summary` and `foo-call-graph.tsv` with whatever tools make sense to you

## Footnotes

[^1]: This isn&rsquo;t completely true, there is the `RuntimeRep` type controls
  exactly this and the levity polymorphism work by [Richard
  Eisenberg](https://richarde.dev/). See [this
  video](https://www.youtube.com/watch?v=Mb_B-j8ePfc) for examples on using these
  features. We do plan to include a more thorough and real world example on using
  levity polymorphism for better performance in the [haskell optimization
  handbook](https://github.com/haskellfoundation/tech-proposals/pull/26).
