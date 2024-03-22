{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = import ./default.nix ;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else if compiler == "profile"
                       then pkgs.profiledHaskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = haskellPackages.callPackage f { inherit (pkgs) gtk3; };
  buildInputsS = [
    haskellPackages.cabal-install
    haskellPackages.haskell-language-server
    pkgs.gtk3
    pkgs.gobject-introspection
    pkgs.glib
  ];

in
  haskellPackages.shellFor {
    packages = p: [drv];
    buildInputs = buildInputsS;
  }

