-- TODO: tutorials/ where we also have some for libraries we use

# SECTIONS

## Summary
- production ready full-stack (frontend, backend, database) template
- multi-platform (Android, iOS, Web)
- any library, module or function is searchable through a local hoogle.

## Haskell Generic Notes?
- Compiler errors are your guiding light
-- See Compiler.md for more details
--- COMPILER.md:
	-- Errors
	-- Build process (nix + ghc810 + ghcjs + ghc(platform))
	--- reflex-platform + obelisk + rhyolite

### Tricks and Extensions used (part 1)

### TABLE OF CONTENTS

# Documentation Note
## tutorials
## hoogle

# Frontend
## Multi-platform Deployments
## Visual Design
### Classh
## Web App
### JavaScript in Haskell
#### JSaddle
#### Reflex-FRP > React : Why use Functional Reactive Programming
##### {Forms} : copy in example from Weather example
## Landing pages
### Tiny HTML + JS Bundles
### Vite-like build process
## Blogs
-- via lamarckian
## Tricks and Extensions used (part 2 ELABORATION)

# Backend
## Generalized Production Level Auth
### OAuth
### Email-Based Auth
#### Login
#### Signup
#### RequestResetPassword
#### ResetPassword
### Subscription Handling Through Stripe
### Role-Based Access Control
## APIs
### jenga-auth + preset auth handlers
### HTTP Req
#### Snap
### JSON Websocket API
### Views + Notify
### Workers
## Config Utils

---> ref: "Config Utils"
### ReaderT pattern
#### Codebase Agnostic Environment
### Startup : Configs m ===> Pure Configs
#### TypeApplications
## Database as a Type
#### Beam
#### Migrations
##### beam-automigrate
## Production-Level Error Reporting

# Deployments
## Server
### ob deploy push
## iOS
### nix-build ios.nix
## Android
### nix-build android.nix
