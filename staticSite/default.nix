{ pkgs ? (import ../. {}).obelisk.nixpkgs
}:
let
  # The nixified node project was generated from a package.json file in src using node2nix
  # See https://github.com/svanderburg/node2nix#using-the-nodejs-environment-in-other-nix-derivations
  nodePkgs = (pkgs.callPackage ./src/node { nodejs = nixos-unstable.nodejs-14_x; }).shell.nodeDependencies;


  p_mmark-cli = import (builtins.fetchGit {
    # Descriptive name to make the store path easier to identify
    name = "my-old-revision";
    url = "https://github.com/NixOS/nixpkgs/";
    ref = "refs/heads/nixpkgs-unstable";
    rev = "05bbf675397d5366259409139039af8077d695ce";
  }) {};

  mmark-cli = p_mmark-cli.haskellPackages.mmark-cli;

  # This is a newer nixpkgs pin to unstable to get a much newer version of node that supports ES6 module imports,
  # this is necessary to use a newer version of tailwind (as we need a newer postcss that requires node 12 or higher)
  nixos-unstable = import (builtins.fetchTarball {
    # Descriptive name to make the store path easier to identify
    name = "nixos-unstable";
    # Commit hash for nixos-unstable as of 2018-09-12
    url = "https://github.com/NixOS/nixpkgs/archive/b67e752c29f18a0ca5534a07661366d6a2c2e649.tar.gz";
    # Hash obtained using `nix-prefetch-url --unpack <url>`
    sha256 = "1n47f7r8cm9pcsz7vl4nxjfvs0fgzvcmjda5h0inz3yx9vghp5xm";
  }) {};

  # The frontend source files have to be passed in so that tailwind's purge option works
  # See https://tailwindcss.com/docs/optimizing-for-production#removing-unused-css
  frontendSrcFiles = ../frontend;

in pkgs.stdenv.mkDerivation {
  name = "static";
  src = ./src;
  buildInputs = [pkgs.nodejs];
  installPhase = ''
  mkdir -p $out/css $out/js $out/images $out/favicons $out/html $out/md-out

  # Node env
  ln -s ${nodePkgs}/lib/node_modules ./node_modules
  export PATH="${nodePkgs}/bin:$PATH"

  # Make frontend sources visible to Tailwind (matches tailwind.config.js)
  ln -s ${frontendSrcFiles} frontend

  # ----- Copy static assets first -----
  cp -r images/* $out/images/
  cp -r js/* $out/js/
  cp -r html/* $out/html/

  # Expose generated HTML at ./html so Tailwind globs pick it up
  ln -s $out/html/ html

  # ----- Build CSS with Tailwind/PostCSS -----
  export NODE_ENV=production
  postcss css/styles.css -o $out/css/styles.css
'';
}
