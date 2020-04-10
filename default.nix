{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, attoparsec, base, hpack, http-client
      , http-client-tls, stdenv, text, time, transformers, turtle
      }:
      mkDerivation {
        pname = "i3blocks-hs-contrib";
        version = "2.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [ base text turtle ];
        libraryToolDepends = [ hpack ];
        executableHaskellDepends = [
          aeson attoparsec base http-client http-client-tls text time
          transformers turtle
        ];
        prePatch = "hpack";
        homepage = "https://github.com/panavtec/i3blocks-hs-contrib#readme";
        description = "Base i3blocks written in haskell";
        license = stdenv.lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
