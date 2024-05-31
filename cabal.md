<!--
Compile the HTML slides with Pandoc:

pandoc -t revealjs -s -o index.html cabal.md -V revealjs-url=reveal.js -V theme=welltyped_theme -V controlsTutorial=false -V controlsLayout=edges -V slideNumber='c/t' -V transition=slide -V width=1100

Custom theme in reveal.js/dist/themes/welltyped_theme.css
-->

---
author: Sam Derbyshire, Well-Typed
title: A new architecture for Cabal
subtitle: Haskell Ecosystem Workshop
date: June 7th, 2024
---

## Plan

- Cabal as a packaging framework: Common Architecture for Building Applications and Libraries.
- The `Setup.hs` interface and its shortcomings.
- The `Cabal` library interface, and how to leverage it (`cabal-install`, HLS).

<p class="indicator">⭲</p>

## Background on Cabal: framework and library

The [Cabal specification](https://www.haskell.org/cabal/proposal/pkg-spec.pdf)
was designed to allow Haskell tool authors to package their code and share it
with other developers.

<br />

<style>
.boxed {
  background: #b0d3f5;
  color: black;
  border: 3px solid #32659ca2;
  margin: 0px auto;
  width: 900px;
  padding: 10px;
  border-radius: 10px;
}
</style>

<div class=boxed>

> The Haskell Package System (Cabal) has the following main goal:
>
>  - to specify a standard way in which a Haskell tool can be packaged, so that it
>    is easy for consumers to use it, or re-package it, regardless of the Haskell
>    implementation or installation platform.

</div>

<p class="indicator">⭲</p>

## Cabal packages

<ul>
  <li>unit of distribution in source format</li>
  <li>enough metadata to make a into system package
    <ul class="fragment">
      <li>package name and version number</li>
      <li>dependencies (e.g. `base >= 4.17 && < 4.21, lens ^>= 5.3`)</li>
      <li>exposed API (libraries, exposed modules, executables...)</li>
      <li>how to build (e.g. `build-type: Simple`)</li>
    </ul>
  </li>
</ul>

<p class="indicator">⭲</p>

## The `Cabal` library

The Cabal library provides

  - data types, parser and pretty printer for the `.cabal` file format
    and the `hc-pkg` *installed package info* format (in `Cabal-syntax`),
  - information about Haskell compilers (e.g. supported Haskell language extensions),
  - the `Setup.hs` CLI,
  - how to build a single package.

<p class="indicator">⭲</p>

## Package registration

A Haskell compiler `hc` supporting the Cabal specification is required to provide
a package registration program, `hc-pkg`. The details of package registration
are laid out in the Cabal specification. The end result is that we store
information about installed libraries.

<!--
TODO: give example of `ghc-pkg describe` and looking at `.conf` files.
-->

Note that, rather confusingly, one does not register packages with this tool,
only individual units.

<p class="indicator">⭲</p>

## The `Setup` interface

To implement Cabal specification, the build system of a package needs only
provide the `Setup` command-line interface, consisting of `Setup` executable
that supports invocations of the form `./Setup <cmd>`.

:::{.element: class="fragment"}

|                     `<cmd>` | description |
| --------------------------: | -----------------------------------------------------------------------|
|                 `configure` | resolve compiler, tools and dependencies |
|    `build`/`haddock`/`repl` | prepare sources and build/generate docs/open a session in the interpreter |
|              `test`/`bench` | run testsuites or benchmarks |
| `install`/`register` | move files into an image dir or final location/register libraries with the compiler |
|                     `sdist` | create an archive for distribution/packaging |
|                     `clean` | clean local files (local package store, local build artifacts, ...) |

<p class="indicator">⭲</p>
:::

## Flags

Each command comes with its own set of flags, e.g.
[`Cabal` `ConfigFlags`](https://github.com/haskell/cabal/blob/3a8c69cb142e27caae5d754ac400636b3417b198/Cabal/src/Distribution/Simple/Setup/Config.hs#L88)
(by far the most complex).

In practice, `./Setup configure` takes many flags, with the configuration
being preserved for subsequenty invocations (which barely take any flags, e.g.
`./Setup build -v2 --builddir=<dir>`).

<p class="indicator">⭲</p>

## Manually building packages with `./Setup`

In a build plan, we must manually build all dependencies in dependency order.

<div class="fragment" data-fragment-index="1">
To build individual units:

<ul>
  <li class="fragment" data-fragment-index="2">`./Setup configure <compName> <confArgs>`</li>
  <li class="fragment" data-fragment-index="3">`./Setup build --builddir=<buildDir>`</li>
  <li class="fragment" data-fragment-index="4">`./Setup haddock --builddir=<buildDir> <haddockArgs>`</li>
  <li class="fragment" data-fragment-index="5">`./Setup copy --builddir=<buildDir> --destDir=<destDir>`</li>
  <li class="fragment" data-fragment-index="6">`./Setup register --builddir=<buildDir> --gen-pkg-config=<unitPkgReg>` (libs only)</li>
  <li class="fragment" data-fragment-index="7">`hc-pkg register --package-db=<pkgDb> <unitPkgRegFile>` (libs only)</li>
</ul>

<p class="indicator" class="fragment" data-fragment-index="7">⭲</p>
</div>

## Invoking `Setup`: the tricky parts

  - passing appropriate arguments to `./Setup configure`
    - `--package-db=<pkgDb>`
    - `--cid=<unitId>`
    - `--dependency=<depPkgNm>:<depCompNm>=<depUnitId>`
  - constructing the correct environment for invoking `./Setup`
    - putting `build-tool-depends` executables in `PATH`
    - defining the corresponding `<buildTool>_datadir` environment variables.

<p class="indicator">⭲</p>

## Versioning of the Setup interface

  - As the Cabal specification evolves, so does the set of flags understood by the
    `Setup` CLI.
  - Care needed when `cabal-install` and the `Setup` executable use a different
    version of the `Cabal` library ([`Distribution.Client.Setup.filterConfigureFlags`](https://github.com/haskell/cabal/blob/3a8c69cb142e27caae5d754ac400636b3417b198/cabal-install/src/Distribution/Client/Setup.hs#L678-L849)).

<p class="indicator">⭲</p>

## Setup.hs too general

Each package brings its own (possibly completely custom) build system,
limiting what `cabal-install` or HLS can do in multi-package projects.

:::{.element: class="fragment"}
In practice, **all** packages use known build systems:

  1. `build-type: Simple`: `./Setup configure` = [`Cabal` library `configure`](https://github.com/haskell/cabal/blob/1b243bd0057c23ad7ed41f6ed60e4c9c77bbc9f0/Cabal/src/Distribution/Simple/Configure.hs#L323), `./Setup build` = [`Cabal` library `build`](https://github.com/haskell/cabal/blob/1b243bd0057c23ad7ed41f6ed60e4c9c77bbc9f0/Cabal/src/Distribution/Simple/Build.hs#L100), etc.
  2. A specific implementation of `build-type: Custom` using
     [`UserHooks`](https://hackage.haskell.org/package/Cabal-3.12.0.0/docs/Distribution-Simple-UserHooks.html#t:UserHooks).

<p class="indicator">⭲</p>
:::

## Example (`singletons-base`)

See [the `Setup.hs` file for `singletons-base`](https://github.com/sheaf/cabal-talk/blob/master/examples/custom/singletons-base/Setup.hs).

<p class="indicator">⭲</p>

##

Instead we would be better off with a **Haskell library interface** for customising
the build system of a package, and make use of this in `cabal-install` instead
of going through the `Setup` CLI.

<p class="indicator">⭲</p>

## Setup hooks

The `Hooks` build-type provides a new way to customise how a package is built:
use the `Cabal` library, but with custom hooks that augment (but don't override)
what the `Cabal` library does.

  - [Haskell Foundation Tech RFC #60](https://github.com/haskellfoundation/tech-proposals/blob/main/rfc/060-replacing-cabal-custom-build.md)
  - [`Cabal-hooks` Haddocks](https://sheaf.github.io/cabal-talk/docs/Cabal-hooks/Distribution-Simple-SetupHooks.html) [(Hackage)](https://hackage.haskell.org/package/Cabal-hooks/docs/Distribution-Simple-SetupHooks.html)
  - [Cabal Hooks overlay](https://gitlab.haskell.org/mpickering/hooks-setup-testing)

<p class="indicator">⭲</p>

## Setup hooks in practice

```txt
cabal-version: 3.14
...
build-type: Hooks
...

custom-setup
  setup-depends:
    base        >= 4.18 && < 5,
    Cabal-hooks >= 0.1  && < 0.2
```


```haskell
module SetupHooks where

-- Cabal-hooks
import Distribution.Simple.SetupHooks

setupHooks :: SetupHooks
setupHooks =
  noSetupHooks
    { configureHooks = myConfigureHooks
    , buildHooks = myBuildHooks }
```

<p class="indicator">⭲</p>

## Configure hooks

<style>
.insert {
  color: #227038;
}
</style>

There are three hooks into the configure phase:

  1. Package-wide pre-configure.  
     `type PreConfPackageHook = PreConfPackageInputs -> IO PreConfPackageOutputs`  
     <div class="fragment insert" class="insert" data-fragment-index="1">custom `./configure`-style logic</div>
  2. Package-wide post-configure.  
     `type PostConfPackageHook = PostConfPackageInputs -> IO ()`
     <div class="fragment insert" data-fragment-index="1">write package-wide information to disk for (3)</div>
  3. Per-component pre-configure.  
     `type PreConfComponentHook = PreConfComponentInputs -> IO PreConfComponentOutputs`  
     <div class="fragment insert" class="insert" data-fragment-index="1">modify components (add exposed modules, specify flags)</div>

<p class="indicator" class="fragment" data-fragment-index="1">⭲</p>

## Configuring a custom preprocessor

See the configure hooks of the [`custom-preproc`](https://github.com/sheaf/cabal-talk/blob/master/examples/hooks/custom-preproc/SetupHooks.hs) example.

<p class="indicator">⭲</p>

## Modifying individual components

See the configure hooks of the [`system-info`](https://github.com/sheaf/cabal-talk/blob/master/examples/hooks/system-info/SetupHooks.hs) example.

<p class="indicator">⭲</p>

## Pre-build rules

The general mechanism for preparing source files for compilation is that of
[**pre-build rules**](https://sheaf.github.io/cabal-talk/docs/Cabal-hooks/Distribution-Simple-SetupHooks.html#t:Rules) [(Hackage)](https://hackage.haskell.org/package/Cabal-hooks/docs/Distribution-Simple-SetupHooks.html#t:Rules).

:::{.element: class="fragment"}
Thinking in terms of custom pre-processors:

  - each rule is a pre-processor invocation with specific arguments,
  - the collection of all custom preprocessors is statically known.

<p class="indicator">⭲</p>
:::

## Example: `singletons-base`, using `Hooks`

See the pre-build rules of the [`singletons-base`](https://github.com/sheaf/cabal-talk/blob/master/examples/hooks/singletons-base/SetupHooks.hs) example.

<p class="indicator">⭲</p>

## Example: custom preprocessors

See the pre-build rules of the [`custom-preproc`](https://github.com/sheaf/cabal-talk/blob/master/examples/hooks/custom-preproc/SetupHooks.hs) example (`myPreBuildRules`).

<p class="indicator">⭲</p>

## Leveraging the library interface

  - compiling hooks to an external executable,
  - versioning of the Hooks API,
  - recompilation checking for pre-build rules,
  - additional challenges of using a library interface in `cabal-install`.

<p class="indicator">⭲</p>

## hooks-exe: the external hooks executable

To integrate packages with `build-type: Hooks` through a library interface,
we compile the `SetupHooks` module into a separate executable
with which we communicates via the CLI.

<!--
TODO: we railed against the CLI of Setup.hs... so why is this CLI OK?
-->

```sh
hooks-exe <inputHandle> <outputHandle> <hookName>
```

*Note:* Uses new `CommunicationHandle` API from [`process`](https://hackage.haskell.org/package/process-1.6.20.0/docs/System-Process-CommunicationHandle.html)

## Versioning

What do we do when `cabal-install` is linked
against a specific version of the `Cabal` library (say `3.16.1.0`) that is
incompatible with the `Cabal` version required by a package with `build-type: Hooks`
(e.g. the package declares `setup-depends: Cabal == 3.14.*` )?

## First line of defense: `Structured`

We use [`Cabal`'s `Structured` mechanism](https://hackage.haskell.org/package/Cabal-syntax-3.12.0.0/docs/Distribution-Utils-Structured.html) to ensure that both sides of the IPC channel
agree on the `Binary` instances used.

## Private dependencies

To provide compatibility between different `Cabal` library versions, we propose
to use **private dependencies**.

:::{.element: class="fragment"}
`cabal-install` would be linked against multiple versions of the `Cabal`
library, and would come bundled with internal adapter functions.
:::

:::{.element: class="fragment"}

```haskell
projectOut_localBuildInfo_V3_16_V3_14
  :: V3_16.LocalBuildInfo -> V3_14.LocalBuildInfo

projectOut_PreConfComponentInputs_V3_16_V3_14
  :: V3_16.PreConfComponentInputs -> V3_14.PreConfComponentInputs

inject_preConfComponentOutputs_V3_14_V3_16
  :: V3_14.PreConfComponentOutputs -> V3_16.PreConfComponentOutputs
```

<p class="indicator">⭲</p>
:::

## hooks-exe: pre-build rules

The external hooks executable supports three queries for pre-build rules:

  - ask for all known rules using the `preBuildRules` hook,  
    this returns a value of type `Map RuleId RuleBinary`;
  - run a dynamic dependency computation,
    this returns additional edges to the build graph of pre-build rules
    as well as extra arguments to be passed to the rule in order to execute it;
  - execute a rule (e.g. run a pre-processor).

:::{.element: class="fragment"}
Details about how e.g. HLS would leverage this are given in
[the `Cabal-hooks` documentation](https://sheaf.github.io/cabal-talk/docs/Cabal-hooks/Distribution-Simple-SetupHooks.html#g:9).

<p class="indicator">⭲</p>
:::


## Process-global state

When [`cabal-install` invokes `./Setup`](https://github.com/haskell/cabal/blob/ab4c13704160f51952dc0d53ecfd10b2feaf6497/cabal-install/src/Distribution/Client/SetupWrapper.hs#L588-L595), it sets a bunch of process-global state.

:::{.element: class="fragment"}
```haskell
  let cp =
        (proc path args)
          { Process.cwd = fmap getSymbolicPath $ useWorkingDir options
          , Process.env = env
          , Process.std_out = loggingHandle
          , Process.std_err = loggingHandle
          , Process.delegate_ctlc = isInteractive options
          }
  maybeExit $ rawSystemProc verbosity cp
```
:::

:::{.element: class="fragment"}

  - working directory ([`7b90583`](https://github.com/haskell/cabal/commit/7b9058328e162a4cb707b5d5b25cd1d2df66680e))
  - environment variables ([`ee11ac6`](https://github.com/haskell/cabal/commit/ee11ac6c7badc452def79116729bd16aea15c0df))
  - logging handle ([`Cabal` #9987](https://github.com/haskell/cabal/issues/9987))

<p class="indicator">⭲</p>
:::

## Making Custom a separate component

- Packages that make use of a custom setup stanza (`Custom` and `Hooks` build types)
  are treated as a whole.
- Locked out of certain features (e.g. multiple sublibraries).

This is a long-standing flaw in the implementation of `cabal-install`.  
The task is up for grabs at [`Cabal` #9986](https://github.com/haskell/cabal/issues/9986).

<p class="indicator">⭲</p>

## End of slides

Slides available online: [sheaf.github.io/cabal-talk](https://sheaf.github.io/cabal-talk).

<br />

Cabal tickets (in increasing order of difficulty):

  - Monitoring directory-recursive file globs in `cabal-install`: [`Cabal` #10064](https://github.com/haskell/cabal/issues/10064)
  - Add support for logging handles to `Cabal`: [`Cabal` #9987](https://github.com/haskell/cabal/issues/9987).
  - Make setup a separate component: [`Cabal` #9986](https://github.com/haskell/cabal/issues/9986).

Also:

  - HLS: add fine-grained recompilation logic for pre-build rules.
