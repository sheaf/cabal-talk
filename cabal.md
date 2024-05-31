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

## Cabal: framework and library

The [Cabal specification](https://www.haskell.org/cabal/proposal/pkg-spec.pdf)
was designed to allow Haskell tool authors to package their code and share it
with other developers.

<p class="indicator">⭲</p>

##

> The Haskell Package System (Cabal) has the following main goal:
>
>  - to specify a standard way in which a Haskell tool can be packaged, so that it  
>    is easy for consumers to use it, or re-package it, regardless of the Haskell  
>    implementation or installation platform.

<p class="indicator">⭲</p>

##

The Cabal concept of a package is a versioned unit of distribution in source
format, with enough metadata to allow it to be translated into system packages
by distribution managers.

<p class="indicator">⭲</p>

##

Each package must bundle some metadata, specified in a `.cabal` file. Chiefly:

  - the package name and version number,
  - its dependencies (e.g. `base >= 4.17 && < 4.21, lens ^>= 5.3`),
  - what the package provides (libraries, exposed modules, executables...),
  - how to build the package (e.g. `build-type: Simple`).

<p class="indicator">⭲</p>

## The `.cabal` file format

<style>
.container{
  display: flex;
}
.col {
  flex: 1;
  margin: 10px;
}
</style>

<div class="container">
<div class="col">
```txt
cabal-version: 3.0
name:        simple-pkg
version:     0.1.0.0
synopsis:    Simple package
category:    Graphics
license:     BSD-3-Clause
homepage:    https://simple.app
author:      Alice
maintainer:  alice@haskell.org
build-type:  Simple
data-dir:    data
data-files:  data1.txt
description: A simple package.
```
</div>
<div class="col">
```txt
flag debug
  description: Debug mode.
  default: False
  manual:  True

common std
    build-depends:
        base
            >= 4.17  && < 4.21
      , containers
            >= 0.6.1 && < 0.8
    ghc-options:
      -Wall
      -Wcompat
    if flag(debug)
      cpp-options:
          -DDEBUG
```
</div>
<div class="col">
```txt
library
    import:
        std
    hs-source-dirs:
        src/lib
    build-depends:
        bytestring
          >= 0.10.1 && < 0.12
    exposed-modules:
        Lib1, Lib2
    other-modules:
        Lib1.Internal
    autogen-modules:
        Paths_pkg
    other-modules:
        Paths_pkg
    default-language:
        Haskell2010
```
</div>
</div>

<p class="indicator">⭲</p>

##

The Cabal library provides

  - data types, parser and pretty printer for the `.cabal` file format
    and the `hc-pkg` *installed package info* format,
  - information about Haskell compilers (e.g. supported Haskell language extensions),
  - the `Setup.hs` CLI,
  - how to build a single package.

<p class="indicator">⭲</p>

##

A Haskell compiler `hc` supporting the Cabal specification is required to provide
a package registration program, `hc-pkg`. The details of package registration
are laid out in the Cabal specification. The end result is that we store
information about installed libraries.

<!--
TODO: give example of `ghc-pkg describe` and looking at `.conf` files.
-->

:::{.element: class="fragment"}
Note that, rather confusingly, one does not register packages with this tool,
only individual units.

<p class="indicator">⭲</p>
:::

## The `Setup` interface

To comply with the Cabal specification, the build system of a package needs only
implement the `Setup` command-line interface, i.e. provide a `Setup` executable
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

##

In practice, the `./Setup configure` command takes a large number of parameters
(as represented in the [`Cabal` `ConfigFlags` data-type](https://github.com/haskell/cabal/blob/3a8c69cb142e27caae5d754ac400636b3417b198/Cabal/src/Distribution/Simple/Setup/Config.hs#L88)).
This configuration is preserved for subsequent invocations, which usually only
take a couple of parameters (e.g. `./Setup build -v2 --builddir=<dir>`).

<p class="indicator">⭲</p>

##

This interface can be used directly to build any package, by executing the
the following recipe:

  - build the dependencies in dependency order,
  - to build each individual unit:
    - `./Setup configure <compName> <confArgs>`
    - `./Setup build --builddir=<buildDir>`
    - `./Setup haddock --builddir=<buildDir> <haddockArgs>` (optional, to generate documentation)
    - `./Setup copy --builddir=<buildDir> --destDir=<destDir>` (this makes executables available, e.g. for `build-tool-depends`)
    - `./Setup register --builddir=<buildDir> --gen-pkg-config=<unitPkgRegFile>` (libraries only)
    - `hc-pkg register --package-db=<pkgDb> <unitPkgRegFile>` (libraries only)

:::{.element: class="fragment"}
The tricky parts in the above are:

  - passing appropriate arguments to `./Setup configure`,
    e.g. `--package-db=<pkgDb>`, `--cid=<unitId>` and
    `--dependency=<depPkgNm>:<depCompNm>=<depUnitId>` arguments,
  - constructing the correct environment for invoking `./Setup`, e.g. adding
    appropriate `build-tool-depends` executables in `PATH` and defining the
    corresponding `<buildTool>_datadir` environment variables.
<p class="indicator">⭲</p>
:::

##

One significant source of complexity in any tool (such as `cabal-install`) which
invokes `Setup` executables is that the `Setup` interface is **versioned**:
as the Cabal specification evolves, so does the set of flags understood by the
`Setup` CLI.  
This means that, when provided with the `Setup` script for a package, one needs
to be careful about what arguments are passed to it; see for instance how
`cabal-install` handles this in [`Distribution.Client.Setup.filterConfigureFlags`](https://github.com/haskell/cabal/blob/3a8c69cb142e27caae5d754ac400636b3417b198/cabal-install/src/Distribution/Client/Setup.hs#L678-L849).
<p class="indicator">⭲</p>

##

The recipe for building a package using `Setup` scripts allows the `Setup`
executables to be implemented in any way the package author chooses; in a way,
each package brings its own build system.

This fundamentally limits what build systems such as `cabal-install` or the
Haskell Language Server can do in multi-package projects, as one has to treat
the build system of each package as an opaque black box.

However, in practice, **all** packages use known build systems:

  1. `build-type: Simple` (most packages).
      This means that `./Setup configure` runs [the `configure` function from the `Cabal` library](https://github.com/haskell/cabal/blob/1b243bd0057c23ad7ed41f6ed60e4c9c77bbc9f0/Cabal/src/Distribution/Simple/Configure.hs#L323),
      `./Setup build` runs [the `build` function from the `Cabal` library](https://github.com/haskell/cabal/blob/1b243bd0057c23ad7ed41f6ed60e4c9c77bbc9f0/Cabal/src/Distribution/Simple/Build.hs#L100), etc.
  2. A specific implementation of `build-type: Custom` using
     [the `Cabal` `UserHooks` mechanism](https://hackage.haskell.org/package/Cabal-3.12.0.0/docs/Distribution-Simple-UserHooks.html#t:UserHooks),
     in which the default logic (e.g. the `Cabal` `build` function) is bracketed
     by custom hooks, e.g. custom configuration code run after
     `Cabal` `configure`, or running an action to generate module sources
     before running `Cabal` `build`.

```haskell
main =
  ( defaultMainWithHooks simpleUserHooks )
    { confHook = \ ( gpd, hbi ) cfgFlags -> do
        gpd' <- customPreConfHook gpd cfgFlags
        confHook simpleUserHooks ( gpd', hbi ) cfgFlags
    }
```

<p class="indicator">⭲</p>

## Example (`singletons-base`)

See [the `Setup.hs` file for `singletons-base`](https://github.com/sheaf/cabal-talk/blob/master/examples/custom/singletons-base/Setup.hs).

<p class="indicator">⭲</p>

##

This motivates the desire for a **Haskell library interface** for customising
the build system of a package. This would allow introspection by build systems,
and allow a gradual restructuring of `cabal-install` away from the `Setup`
command-line interface, which has grown unwieldy due the needs of package managers.

:::{.element: class="fragment"}
What's required is then:

  - to provide a library interface to add hooks around these phases, so that
    packages with `build-type: Custom` can be migrated to use a library interface,
  - make use of this library interface in `cabal-install`, as opposed to going
    through the `Setup` CLI.

<p class="indicator">⭲</p>
:::

## Setup hooks

The `Hooks` build-type provides a new way to customise how a package is built:
use the `Cabal` library, but with custom hooks that augment (but don't override)
what the `Cabal` library does.

  - [Haskell Foundation Tech RFC #60](https://github.com/haskellfoundation/tech-proposals/blob/main/rfc/060-replacing-cabal-custom-build.md),
  - [`Cabal-hooks` Haddocks](https://sheaf.github.io/cabal-talk/docs/Cabal-hooks/Distribution-Simple-SetupHooks.html) [(Hackage)](https://hackage.haskell.org/package/Cabal-hooks/docs/Distribution-Simple-SetupHooks.html),
  - [Cabal Hooks overlay](https://gitlab.haskell.org/mpickering/hooks-setup-testing).

<p class="indicator">⭲</p>

## Setup hooks in practice

```cabal
cabal-version: 3.14
...
build-type: Hooks
...

custom-setup
  setup-depends:
    base        >= 4.18 && < 5,
    Cabal-hooks >= 0.1  && < 0.2
```

<p class="indicator">⭲</p>

##

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

There are three hooks into the configure phase:

  1. Package-wide pre-configure.  
     `type PreConfPackageHook = PreConfPackageInputs -> IO PreConfPackageOutputs`
  2. Package-wide post-configure.  
     `type PostConfPackageHook = PostConfPackageInputs -> IO ()`
  3. Per-component pre-configure.  
     `type PreConfComponentHook = PreConfComponentInputs -> IO PreConfComponentOutputs`

:::{.element: class="fragment"}
(1) can be used for custom `./configure`-style logic, e.g. configuring programs
for use when building the package that don't fit into the framework of Cabal's
`build-tool-depends` (e.g. because they require custom logic for parsing their
version number).

(2) can be used to write custom package-wide information to disk, to be consumed
by (3).

(3) can be used to modify individual components, e.g. adding exposed modules
or specifying flags to be used when building the component.

<p class="indicator">⭲</p>
:::


## Configuring a custom preprocessor

The configure hooks of the [`custom-preproc`](https://github.com/sheaf/cabal-talk/blob/master/examples/hooks/custom-preproc/SetupHooks.hs) example.

<p class="indicator">⭲</p>

## Modifying individual components

The configure hooks of the [`system-info`](https://github.com/sheaf/cabal-talk/blob/master/examples/hooks/system-info/SetupHooks.hs) example.

<p class="indicator">⭲</p>

## Pre-build rules

The general mechanism for preparing source files for compilation is that of
[**pre-build rules**](https://sheaf.github.io/cabal-talk/docs/Cabal-hooks/Distribution-Simple-SetupHooks.html#t:Rules)[(Hackage)](https://hackage.haskell.org/package/Cabal-hooks/docs/Distribution-Simple-SetupHooks.html#t:Rules).

:::{.element: class="fragment"}
Thinking in terms of custom pre-processors:
  - each rule is a pre-processor invocation with specific arguments,
  - the collection of all custom preprocessors is statically known.
:::

## Example: `singletons-base`, using `Hooks`

The pre-build rules of the [`singletons-base`](https://github.com/sheaf/cabal-talk/blob/master/examples/hooks/singletons-base/SetupHooks.hs) example.

<p class="indicator">⭲</p>

## Example: custom preprocessors

The pre-build rules of the [`custom-preproc`](https://github.com/sheaf/cabal-talk/blob/master/examples/hooks/custom-preproc/SetupHooks.hs) example (`myPreBuildRules`).

<p class="indicator">⭲</p>

## Leveraging the library interface

We saw above how to migrate packages with custom build logic to use `build-type: Hooks`,
adding a layer of customisation to the Cabal library interface. This allows all
kinds of packages to be built directly with a library interface, without going
through the `Setup` CLI.

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

More specifically, the question is: what do we do when `cabal-install` is linked
against a specific version of the `Cabal` library (say `3.16.1.0`) that is
incompatible with the `Cabal` version required by a package with `build-type: Hooks`
(e.g. the package declares `setup-depends: Cabal == 3.14.*` )?

## Structured

As a first line of defense, we use [`Cabal`'s `Structured` mechanism](https://hackage.haskell.org/package/Cabal-syntax-3.12.0.0/docs/Distribution-Utils-Structured.html),
which ensures that both sides of the IPC channel agree on the `Binary` instances
used for serialisation and deserialisation.

## Private dependencies

To provide compatibility between different `Cabal` library versions, we propose
to use **private dependencies**.

:::{.element: class="fragment"}
`cabal-install` would be linked against multiple versions of the `Cabal`
library, and would come bundled with internal adapter functions, e.g.:
:::

:::{.element: class="fragment"}

```haskell
project @[3,16] @[3,14] @LocalBuildInfo
  :: Cabal.V3_16.LocalBuildInfo -> Cabal.V3_14.LocalBuildInfo

project @[3,16] @[3,14] @Component
  :: Cabal.V3_16.Component -> Cabal.V3_14.Component

project @[3,16] @[3,14] @PreConfComponentInputs
  :: Cabal.V3_16.PreConfComponentInputs -> Cabal.V3_14.PreConfComponentInputs

inject @[3,14] @[3,16] @PreConfComponentOutputs
  :: Cabal.V3_14.PreConfComponentOutputs -> Cabal.V3_16.PreConfComponentOutputs
```

<p class="indicator">⭲</p>
:::

## hooks-exe: pre-build rules

Pre-build rules are a bit more complex, especially as they are involved in
recompilation checking. The external hooks executable supports three queries:

  - ask for all known rules using the `preBuildRules` hook,  
    this returns a value of type `Map RuleId RuleBinary`;
  - run a dynamic dependency computation,
    this returns additional edges to the build graph of pre-build rules
    as well as extra arguments to be passed to the rule in order to execute it;
  - execute a rule (e.g. run a pre-processor).

For recompilation checking, the parent build system keeps track of
the most recent set of rules `Map RuleId RuleBinary`. The build system must then
ensure the rules are **up-to-date**, and, when that is the case, it must
must execute of all the (demanded) **stale rules**.

The set of rules as a whole is considered **out-of-date** precisely when any
one of the following conditions apply:

<dl>
  <dt>O1</dt>
  <dd>there was a monitored change in the files and directories
      monitored by the rules,</dd>

  <dt>O2</dt>
  <dd>the environment passed to the computation of rules has changed.</dd>

</dl>

If the rules are out-of-date, the build system is expected to re-run the
computation that computes all rules.  
After this re-computation of the set of all rules, we match up new rules
with old rules, by `RuleId`. A rule is then considered __stale__ if any of
following conditions apply:

<dl>
  <dt>N</dt>
  <dd>the rule is new (and thus has never been executed), or</dd>
  <dt>S</dt>
  <dd>the rule matches with an old rule, and either:
    <dl>
    <dt>S1</dt>
    <dd>a file dependency of the rule has been modified/created/deleted, or
        a (transitive) rule dependency of the rule is itself stale, or</dd>
    <dt>S2</dt>
    <dd>the rule is different from the old rule, e.g. the argument stored in
        the rule command has changed, or the pointer to the action to run the
        rule has changed. (This is determined using the <code>Eq RuleBinary</code>
        instance.)
    </dd>
    </dl>
  </dd>
</dl>

A stale rule becomes no longer stale once we run its associated action
(which will require running the rule's dynamic dependency computation first, if
it has one). The build system is responsible for re-running the actions
associated with each demanded stale rule, in dependency order (including dynamic
dependencies).

## Additional challenges

`Distribution.Client.SetupWrapper.invoke`:

```haskell
  let loggingHandle = case useLoggingHandle options of
        Nothing -> Inherit
        Just hdl -> UseHandle hdl
      cp =
        (proc path args)
          { Process.cwd = fmap getSymbolicPath $ useWorkingDir options
          , Process.env = env
          , Process.std_out = loggingHandle
          , Process.std_err = loggingHandle
          , Process.delegate_ctlc = isInteractive options
          }
  maybeExit $ rawSystemProc verbosity cp
```

:::{.element: class="fragment"}

  - working directory ([`7b90583`](https://github.com/haskell/cabal/commit/7b9058328e162a4cb707b5d5b25cd1d2df66680e)),
  - environment variables ([`ee11ac6`](https://github.com/haskell/cabal/commit/ee11ac6c7badc452def79116729bd16aea15c0df)),
  - logging handle [`Cabal` #9987](https://github.com/haskell/cabal/issues/9987).
:::

## Make Custom a separate component

Another piece of further work is that packages that make use of a custom setup
stanza (which currently includes `Custom` and `Hooks` build types) are treated
as a whole, not benefiting from the "per component" logic in `cabal-install` and
thus being locked out of certain features such as multiple sublibraries. This is
a long-standing flaw in the implementation of `cabal-install`. The task is
up for grabs at [`Cabal` #9986](https://github.com/haskell/cabal/issues/9986).

## HLS integration

Finally, there is the task of integrating the fine-grained recompilation logic
for pre-build rules outlined in [§ hooks-exe: pre-build rules](#hooks-exe-pre-build-rules)
into the Haskell Language Server.

## End of slides

Slides available online: [sheaf.github.io/cabal-talk](https://sheaf.github.io/cabal-talk).

Cabal tickets (in increasing order of difficulty):

  - Monitoring directory-recursive file globs in `cabal-install`: [`Cabal` #10064](https://github.com/haskell/cabal/issues/10064)
  - Add support for logging handles to `Cabal`: [`Cabal` #9987](https://github.com/haskell/cabal/issues/9987).
  - Make setup a separate component: [`Cabal` #9986](https://github.com/haskell/cabal/issues/9986).
