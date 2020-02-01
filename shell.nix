{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, async, base, bytestring, containers, dbus
      , gi-gtk, gi-gtk-hs, gtk3, mtl, req, stdenv, stm, tagsoup, text
      , transformers
      }:
      mkDerivation {
        pname = "musicScroll";
        version = "0.1.1.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        enableSeparateDataOutput = true;
        libraryHaskellDepends = [
          async base bytestring containers dbus gi-gtk gi-gtk-hs mtl req stm
          tagsoup text transformers
        ];
        executableHaskellDepends = [ base ];
        executablePkgconfigDepends = [ gtk3 ];
        homepage = "https://github.com/RubenAstudillo/MusicScroll";
        description = "Supply your tunes info without leaving your music player";
        license = stdenv.lib.licenses.gpl3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f { gtk3 = nixpkgs.gtk3; });

in

  if pkgs.lib.inNixShell then drv.env else drv
