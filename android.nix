# Builds an unsigned release bundle for Android.
# The result can be signed using jarsigner.
# The options that can be set below are documented in reflex-platform/android/default.nix
with (import ./. {});
android.frontend.override (drv: {
  isRelease = true;
  gradleTask = "bundleRelease"; # This will produce an AAB file (android application bundle) which is the format expected by the Google Play Store
  # NB: We DO NOT USE the releaseKey argument from reflex-platform/android/default.nix
  # because we want to keep our keys out of the nix store
})
