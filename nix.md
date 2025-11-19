# Nix usage in Jenga

## Motivation/Summary

Nix is a comprehensive solution to package management and building, both making it easy to work with any arbitrary package and easily create reproducible builds. While it is currently seen as difficult to learn, it makes it easy for us to build a framework where with one command, we can create bug-free cross-platform builds. This is largely due to the nix ecosystem. For instance, if I setup my environment for data analysis, regardless of what language we are using, I can describe my build declaratively through the nix language. If you have nix, then you can simply run

```bash
nix-shell myBuild.nix
```

and you will have a working build that mirrors mine perfectly.

This has given rise to individual ecosystems like reflex-platform which make it easy to get up and running with Haskell web-development. Similarly, Obelisk builds off reflex-platform, and Jenga builds off Obelisk and other ecosystems. This includes both ecosystems which actively use nix and those which only use a given compiler or interpreter, such as the [mmmark package set](https://github.com/mmark-md) as it was trivial to include these ecosystems with knowledge of nix.

Obelisk + reflex-platform's core value to us is the ability to easy build

- our deployed Linux+nginx server
- our web app
- our web app (as WebAssembly)
- to android
- to iOS
- dev on MacOS
- dev on Linux
- dev on Windows WSL
- all of our tests
- documentation through a local [Hoogle](https://hoogle.haskell.org/)

All through default.nix

As seen in our README.md, all that you need to do once you've installed Nix + Obelisk is run

```haskell
ob run
```

## Tooling Stack: Nix, Obelisk, and reflex-platform

This framework is built on the same foundations used by the Obsidian Systems / Reflex ecosystem: **Nix** for reproducible builds, **Obelisk** for full-stack Haskell apps, and **reflex-platform** for cross-platform Haskell tooling and FRP UIs. The short version: **one Haskell codebase, multiple targets, deterministic builds.**

### Nix Reproducible, Declarative Builds

We rely on **Nix** as the primary build and dependency layer.

Nix is a *pure, functional* package manager and build tool: packages are built by side-effect-free functions and stored in an immutable store. This gives us reproducible builds, isolation between dependencies, and the guarantee that if something builds on one machine it will build the same way on another. ([nixos.org][1])

On top of that, we use Nix to:

* Define development environments declaratively (toolchains, libraries, system deps).
* Support multi-platform and even distributed builds (e.g. offloading work to other machines or architectures).
* Get reliable, atomic upgrades/rollbacks and cached binaries from Nixpkgs and public caches.

In practice, this means contributors can clone the repo, run the provided Nix commands, and land in a fully configured environment with the right GHC, Reflex, Obelisk, Android/iOS SDKs (where applicable), and system libraries, no manual setup.

Thanks to Haskell-specific contributions to Nix, we can also run a local hoogle that gives us a full searchable index of every haskell function and type from libraries we have as dependencies in the Jenga ecosystem.

### Obelisk Full-Stack Haskell, Web & Mobile

**Obelisk** is our full-stack framework layer. It's a tool and library stack for building [functionally reactive](https://en.wikipedia.org/wiki/Functional_reactive_programming) web and mobile applications in Haskell, with "batteries included." ([GitHub][4])

We use Obelisk for:

* A unified Haskell codebase that can target **web, iOS, and Android** from the same project.
* Shared routing and types between frontend and backend, so the API boundary is type-checked end-to-end.
* A curated, production-oriented stack (build system, routing, deployment story) rather than assembling everything by hand.

Obelisk itself is built on Nix for dependency management and deployment. This framework plugs into that model so we can reuse the same deployment and caching story while layering our own conventions and modules on top.

### reflex-platform for Cross-Platform Haskell

Where Obelisk gives us the full-stack framework, **reflex-platform** provides the underlying curated Haskell toolchain and multi-platform build.

Officially, reflex-platform is *"a curated package set and set of tools that let you build Haskell packages so they can run on a variety of platforms,"* built on top of Nix. ([GitHub][7])

We use reflex-platform to:

* Pin a known-good set of Reflex/Reflex-DOM and related libraries that are **tested together**.
* Take advantage of **binary caches** so contributors rarely compile the world from scratch.
* Target multiple platforms from the same codebase:

  * JavaScript in the browser
  * Mobile (iOS/Android)
  * Desktop (Linux/macOS) ([GitHub][7])
* Get convenient tooling such as Nix shells, `work-on`/`try-reflex` scripts, and local Hoogle/Haddock environments for development. ([GitHub][7])

[1]: https://nixos.org/ "Nix & NixOS | Declarative builds and deployments"
[2]: https://releases.nixos.org/nix/nix-1.11.1/manual/index.html "Nix Package Manager Guide"
[3]: https://nixos.org/explore/ "Explore | Nix & NixOS"
[4]: https://github.com/obsidiansystems/obelisk "obsidiansystems/obelisk: Functional reactive web and ..."
[5]: https://blog.obsidian.systems/shaping-a-crud-app-in-haskell-for-powerzonepack/ "Building a Haskell CRUD stack with Obelisk for ..."
[6]: https://blog.obsidian.systems/page/2/ "Obsidian Systems (Page 2)"
[7]: https://github.com/reflex-frp/reflex-platform "reflex-frp/reflex-platform"
[8]: https://reflex-frp.org/get-started "Get Started"
