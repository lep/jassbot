{
  description = "jassbot - jass cli util";

  inputs = {
    # nixpkgs.url = "github:NixOS/nixpkgs";
    commonj.url = "github:lep/common-j";
    commonj.inputs.nixpkgs.follows = "nixpkgs";
    commonj.inputs.flake-utils.follows = "flake-utils";
    flake-utils.url = "github:numtide/flake-utils";
    # flake-utils.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, flake-utils, commonj }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        cj = commonj.packages.${system}.common-j;
        pkgs = import nixpkgs { inherit system; };
        ghcPackages = pkgs.haskellPackages.ghcWithPackages (ps: [
          ps.optparse-applicative
          ps.megaparsec
          ps.utf8-string
          ps.network
          ps.warp
          ps.wai
          ps.HostAndPort
          ps.aeson
          ps.http-types
          # ps.snap
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

        docker = pkgs.dockerTools.buildLayeredImage {
          name = "jassbot-api";
          tag = "latest";
          config = { Entrypoint = [ "${web}/bin/web" "${cj}/common.j" ]; };
        };

        nixosModule = { config, lib, pkgs, ... }:
          let cfg = config.jassbot.services.api;
          in {
            options.jassbot.services.api = {
              enable = lib.mkEnableOption "Enable jassbot web API"; # TODO: web options
            };

            config = lib.mkIf cfg.enable {
              systemd.services."jassbot.api" = {
                wantedBy = [ "multi-user.target" ];
                serviceConfig = {
                  ExecStart = "${web}/bin/web ${cj}/common.j";
                };
              };
            };
            
          };
          

      in {
        packages.j = j;
        packages.web = web;
        packages.docker = docker;
        defaultPackage = j;
        nixosModules.default = nixosModule;

        devShell = pkgs.mkShell {
          buildInputs = [ ghcPackages pkgs.cabal-install ];
          inputsFrom = builtins.attrValues self.packages.${system};
        };
      });
}
