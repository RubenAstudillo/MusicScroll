{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, async, base, bytestring, containers
      , contravariant, cryptonite, dbus, deepseq, directory, gi-gtk
      , gi-gtk-hs, gtk3, hspec, mtl, pipes, pipes-concurrency, req
      , sqlite-simple, stdenv, stm, tagsoup, text, transformers
      , xdg-basedir
      }:
      mkDerivation {
        pname = "musicScroll";
        version = "0.3.2.1";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        enableSeparateDataOutput = true;
        libraryHaskellDepends = [
          async base bytestring containers contravariant cryptonite dbus
          deepseq directory gi-gtk gi-gtk-hs mtl pipes pipes-concurrency req
          sqlite-simple stm tagsoup text transformers xdg-basedir
        ];
        executableHaskellDepends = [ base ];
        executablePkgconfigDepends = [ gtk3 ];
        testHaskellDepends = [ base hspec text ];
        homepage = "https://github.com/RubenAstudillo/MusicScroll";
        description = "Supply your tunes info without leaving your music player";
        license = stdenv.lib.licenses.gpl3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else if compiler == "profile"
                       then pkgs.profiledHaskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f { gtk3 = nixpkgs.gtk3; });

in

  if pkgs.lib.inNixShell then drv.env else drv
