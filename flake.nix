{
  description = "Quickly get information from Hydra.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    devshell.url = "github:numtide/devshell/";
    flake-compat = { url = "github:edolstra/flake-compat"; flake = false; };
    flake-utils.url = "github:numtide/flake-utils";
    devshell.inputs.nixpkgs.follows = "nixpkgs";
    devshell.inputs.flake-utils.follows = "flake-utils";
  };

  outputs = { self, devshell, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgName = "hydra-tools";
        pkgs = nixpkgs.legacyPackages.${system};
        hPkgs = pkgs.haskellPackages;
        inherit (devshell.legacyPackages.${system}) mkShell;

        pkg = hPkgs.callCabal2nix pkgName ./. { };
      in
      {
        # Built by `nix build .`
        packages.${pkgName} = pkg;
        packages.default = pkg;

        # # This is used by `nix develop .`
        devShell = mkShell {
          name = "hydra-tools-devshell";
          packages = pkgs.lib.attrValues {
            inherit (hPkgs)
              haskell-language-server
              implicit-hie
              # weeder
              ;

            inherit (pkgs)
              stack
              hlint
              ;
          };
          commands = [
            {
              help = "Regenerate hie.yaml (run from project root)";
              name = "hie";
              category = "project";
              command = "gen-hie > hie.yaml";
            }
          ];
        };
      }
    );
}

