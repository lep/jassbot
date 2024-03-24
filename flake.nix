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
              enable = lib.mkEnableOption "Enable jassbot web API";

              num-results = lib.mkOption {
                description = "How many results to show at most";
                type = lib.types.ints.positive;
                default = 40;
              };

              threshold = lib.mkOption {
                description = "Minimum score to be displayed";
                type = lib.types.numbers.between 0 1;
                default = 0.4;
              };

              address = lib.mkOption {
                description = "Connection string";
                default = "127.0.0.1:3000";
                example = "/var/run/unix.sock";
                type = lib.types.str;
              };

              jass-files = lib.mkOption {
                description = "List of jass files to parse";
                default = [ "${cj}/common.j" ];
                type = lib.types.listOf lib.types.path;
              };

            };

            config = lib.mkIf cfg.enable {
              systemd.services."jassbot.api" = {
                wantedBy = [ "multi-user.target" ];
                serviceConfig = {
                  ExecStart = "${web}/bin/web --threshold ${toString cfg.threshold} --num-results ${toString cfg.num-results} --address ${toString cfg.address} ${toString cfg.jass-files}";
                };
              };
            };
            
          };
          

      in {
        packages.j = j;
        packages.web = web;
        packages.docker = docker;
        defaultPackage = j;

        # TODO: this is scoped under system
        nixosModules.default = nixosModule;

        devShell = pkgs.mkShell {
          buildInputs = [ ghcPackages pkgs.cabal-install ];
          inputsFrom = builtins.attrValues self.packages.${system};
        };
      });
}
