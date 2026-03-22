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
    terms.security.acme.acceptTerms = true;
  }
}:
with obelisk;
project ./. ({ pkgs, hackGet, ... }@args:
  let
    haskellLib = pkgs.haskell.lib;
  in
    {
      packages = {
        landing-page = (hackGet ./landing-page);
      };
      overrides = pkgs.lib.composeExtensions
        (pkgs.callPackage (hackGet ./thunks/rhyolite) args).haskellOverrides
        (self: super: with pkgs.haskell.lib; {
          rhyolite-beam-task-worker-backend = dontCheck (super.rhyolite-beam-task-worker-backend);

          obelisk-oauth-backend = super.callCabal2nix "obelisk-oauth-backend" (hackGet ./thunks/obelisk-oauth + "/backend") {};
          obelisk-oauth-common = super.callCabal2nix "obelisk-oauth-common" (hackGet ./thunks/obelisk-oauth + "/common") {};

          jenga-auth-frontend = super.callCabal2nix "jenga-auth-frontend" ( hackGet ./thunks/jenga-auth + "/jenga-auth-frontend" ) {};
          jenga-auth-common = super.callCabal2nix "jenga-auth-common" ( hackGet ./thunks/jenga-auth + "/jenga-auth-common")  {};
          jenga-auth-backend = super.callCabal2nix "jenga-auth-backend" ( hackGet ./thunks/jenga-auth + "/jenga-auth-backend" ) {};

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
