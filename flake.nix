# SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io/>
#
# SPDX-License-Identifier: CC0-1.0

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

        haskellPackages = pkgs.haskellPackages;

        jailbreakUnbreak = pkg:
          pkgs.haskell.lib.doJailbreak (pkg.overrideAttrs (_: { meta = { }; }));

        # DON'T FORGET TO PUT YOUR PACKAGE NAME HERE, REMOVING `throw`
        packageName = "jassbot";
        cabalBuilds = haskellPackages.callCabal2nix packageName self rec {
            # Dependency overrides go here
        };
      in rec {
        packages.${packageName} = cabalBuilds;
        defaultPackage = cabalBuilds;

        apps.web = { type = "app"; program = "${cabalBuilds}/bin/web"; };
        apps.j = { type = "app"; program = "${cabalBuilds}/bin/j"; };
        apps.default = apps.j;

        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            #haskellPackages.haskell-language-server # you must build it with your ghc to work
            #ghcid
            cabal-install
          ];
          inputsFrom = builtins.attrValues self.packages.${system};
        };
      });
}
