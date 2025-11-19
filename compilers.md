# GHCJS

For building web application frontend. Current version is 8.10. Learn more [GHCJS](https://github.com/ghcjs/ghcjs)

This results in all.js file that will be used by the live application.

# GHC

Primarily for building server. Current version is 8.10. Learn more [GHC](https://downloads.haskell.org/ghc/latest/docs/users_guide/)

This is also used in local development and changes the way that the JavaScript frontend is evaluated. [jsaddle-warp](https://hackage.haskell.org/package/jsaddle-warp) is used to interpret JavaScript code from Haskell and to avoid lengthy compile times.

# Backend vs Frontend

It is important to note that some packages will not build on frontend, at least currently. This is because the package in question has declared itself unbuildable by that platform/compiler. For the most part these are quite reasonable to understand why they cannot be used on the frontend context which is compiled to javascript. For example, you cannot compile postgres packages through ghcjs. [http-client](https://hackage.haskell.org/package/http-client) might be one case for confusion with beginner-users as while it can make intuitive sense, we need to use XHR in a browser context. For this XHR use-case see [Reflex.Dom.Xhr](https://hackage-content.haskell.org/package/reflex-dom-core-0.8.1.4/docs/Reflex-Dom-Xhr.html).

This limitation may change in light of recent improvements to GHC version 9.\* and it's WebAssembly backend.

# iOS

todo:explain, for now see https://github.com/reflex-frp/reflex-platform/blob/develop/docs/platform-support.md

# Android

Uses a ghc configured to cross compile to Android's architecture.

for more see https://github.com/reflex-frp/reflex-platform/blob/develop/docs/platform-support.md

The main function of frontend is required to be in Java, however we can compile everything else down to C and access it through the JNI. [android-activity](https://github.com/obsidiansystems/android-activity) is the base which we build our application from.

# Landing Pages

Lamarckian is the package we use developing a landing-page. We use the same GHC as for our server however it's important to note we do a fair bit of metaprogramming through template haskell at compile time to deliver a fast build+service in local development and fast service in production.

# MMark as a Compiler

Markdown is worth noting here, as it's both an input to lamarckian builds and a compiler for markdown text. Using MMark is an opinionated choice we made, in that it is very 'strict' about markdown errors. For instance if you have an invalid link, it will complain so that you know to fix it, instead of a user getting frustrated by your typo.

# Future State

GHC version 9 has some truly incredible updates. It is on the roadmap to upgrade to this version and will drastically change the internals of Jenga.
