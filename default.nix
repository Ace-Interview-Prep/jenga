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
      overrides = self: super: with pkgs.haskell.lib; {
        backend = haskellLib.overrideCabal super.backend {
          enableSeparateDataOutput = false;
        };
      };
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
