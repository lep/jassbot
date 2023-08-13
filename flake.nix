{
  description = "jassbot - jass cli util";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        ghcPackages = pkgs.haskellPackages.ghcWithPackages(ps: [
          ps.optparse-applicative
          ps.megaparsec
        ]);

        j = pkgs.stdenv.mkDerivation {
          name = "j";
          src = self;
          buildPhase = ''
            ${ghcPackages}/bin/ghc -O j
          '';

          installPhase = ''
            install -Dt $out/bin j
          '';
        };

        web = pkgs.stdenv.mkDerivation {
          name = "web";
          src = self;
          buildPhase = ''
            ${ghcPackages}/bin/ghc -O web
          '';

          installPhase = ''
            install -Dt $out/bin web
          '';
        };

      in {
        packages.j = j;
        packages.web = web;
    		defaultPackage =  j;

    		devShell = pkgs.mkShell {
    		    buildInputs =  [ ghcPackages pkgs.cabal-install ];
    		    inputsFrom = builtins.attrValues self.packages.${system};
    		};
      });
}
