{ system ? builtins.currentSystem
, obelisk ? import .obelisk/impl {
    inherit system;
    iosSdkVersion = "13.2";
    config = {
      allowBroken = true;
    };
    # You must accept the Android Software Development Kit License Agreement at
    # https://developer.android.com/studio/terms in order to build Android apps.
    # Uncomment and set this to `true` to indicate your acceptance:
    config.android_sdk.accept_license = false;
    # In order to use Let's Encrypt for HTTPS deployments you must accept
    # their terms of service at https://letsencrypt.org/repository/.
    # Uncomment and set this to `true` to indicate your acceptance:
    terms.security.acme.acceptTerms = false;
  }
}:
with obelisk;
project ./. ({ pkgs, hackGet, ... }@args:
  let
    thunkSet = pkgs.thunkSet ./thunks;
    jengaThunkSet = pkgs.thunkSet ./thunks/jenga-auth;
    obelisk-oauth-thunkSet = pkgs.thunkSet ./thunks/obelisk-oauth;
    haskellLib = pkgs.haskell.lib;
    reflex-dom-echartsSrc = pkgs.fetchFromGitHub {
      owner = "augyg";
      repo = "reflex-dom-echarts";
      rev = "3d4941f2be647fd7475b957b5586eee6b6dccf36";
      sha256 = "sha256-qCQzZthpc0Q3TeQPEru1KBM5FooIsz7xRrw1Uvf5hV8=";
    };
    echarts-jsdomSrc = pkgs.fetchFromGitHub {
      owner = "augyg";
      repo = "echarts-jsdom";
      rev = "aaffb109ef01a449b36bb6d27be8111bb72ae0dc";
      sha256 = "sha256-RHzKD+LBs6DkNlGwd9Xnh8VIbygN6GCEnHmtezbgUHA=";
    };
    network-uriSrc = pkgs.fetchFromGitHub {
      owner = "Ace-Interview-Prep";
      repo = "network-uri";
      rev = "47534c4e4c063a566bd0d8db5570a060eaecc656";
      sha256 = "sha256-uPb8vFzbp7PIH2sgLpqoIg75+QRt1CPvwhSWhG4YVYs=";
    };

    vesselSrc = pkgs.fetchFromGitHub {
      owner = "Ace-Interview-Prep";
      repo = "vessel";
      rev = "e17794f9d0784242531d0c5f7ce084dcccbe5813";
      sha256 = "sha256-DumHQwR9CBNA+520w7w+i/IbIdbVPImJupN3YJcrJIw=";
    };
  in
    {
      packages = {
        landing-page = (hackGet ./landing-page);
      };
      overrides = pkgs.lib.composeExtensions
        (pkgs.callPackage (hackGet ./thunks/rhyolite) args).haskellOverrides
        (self: super: with pkgs.haskell.lib; {
          stripe-core = doJailbreak super.stripe-core;
          stripe-tests = doJailbreak super.stripe-tests;
          stripe-http-client = doJailbreak super.stripe-http-client;


          text-metrics = self.callHackage "text-metrics" "0.3.0" {};
          skylighting = dontHaddock (self.callHackage "skylighting" "0.10.2" {});
          skylighting-core = dontHaddock (self.callHackage "skylighting-core" "0.10.2" {});
          ghc-lib-parser = dontHaddock (super.ghc-lib-parser);
          ghc-syntax-highlighter = dontHaddock (self.callHackage "ghc-syntax-highlighter" "0.0.6.0" {});
          IStr = super.callPackage thunkSet.IStr {};
          lamarckian = self.callCabal2nix "lamarckian" (hackGet thunkSet.lamarckian) {};
          bytestring-aeson-orphans = doJailbreak (super.bytestring-aeson-orphans);
          beam-automigrate = doHaddock (super.beam-automigrate);
          rhyolite-beam-task-worker-backend = dontCheck (super.rhyolite-beam-task-worker-backend);
          network-uri = super.callCabal2nix "network-uri" (network-uriSrc) {};
          ClasshSS = super.callPackage (thunkSet.ClasshSS) {};
          parseargs = dontCheck (super.parseargs);

          obelisk-oauth-backend = super.callCabal2nix "obelisk-oauth-backend" (hackGet ./thunks/obelisk-oauth + "/backend") {};
          obelisk-oauth-common = super.callCabal2nix "obelisk-oauth-common" (hackGet ./thunks/obelisk-oauth + "/common") {};

          jenga-auth-frontend = super.callCabal2nix "jenga-auth-frontend" ( hackGet ./thunks/jenga-auth + "/jenga-auth-frontend" ) {};
          jenga-auth-common = super.callCabal2nix "jenga-auth-common" ( hackGet ./thunks/jenga-auth + "/jenga-auth-common")  {};
          jenga-auth-backend = super.callCabal2nix "jenga-auth-backend" ( hackGet ./thunks/jenga-auth + "/jenga-auth-backend" ) {};

          reflex-classhss = super.callPackage thunkSet.reflex-classh {};
          reflex-dom-echarts = self.callCabal2nix "reflex-dom-echarts" reflex-dom-echartsSrc {};
          echarts-jsdom = self.callCabal2nix "echarts-jsdom" echarts-jsdomSrc {};
          templates = self.callCabal2nix "templates" thunkSet.templates {};
          vessel = self.callCabal2nix "vessel" vesselSrc {};
          snap-extras = doJailbreak (super.snap-extras);
          scrappy-core = super.callPackage (thunkSet.scrappy-core) {};
          scrappy-template = super.callPackage (thunkSet.scrappy-template) {};
          gargoyle-postgresql-nix = haskellLib.overrideCabal super.gargoyle-postgresql-nix {
            librarySystemDepends = [ pkgs.postgresql_11 ];
          };
          backend = haskellLib.overrideCabal super.backend {
            enableSeparateDataOutput = false;
          };
        });
      staticFiles = {
        staticAssets = {
          path = ./static;
          isDrv = true;
          drvArgs = { inherit pkgs; };
          moduleName = "Obelisk.Generated.Static";
          ## packageName
          ## functionPrefix
          ## androidInclude
          ## iosInclude
          ## sourcePath :: T.Text -- in Written .hs file
          ## packageName :: T.Text -- in Written .hs file
          ## hotReload ? true
        };
        staticSite = {
          path = ./staticSite;
          isDrv = true;
          drvArgs = { inherit pkgs; };
          moduleName = "Landing.Static";
        };
      };
      android.applicationId = "jenga.app";
      android.displayName = "Jenga App";
      ios.bundleIdentifier = "jenga.app";
      ios.bundleName = "Jenga App";
    }
)
