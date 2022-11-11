let pkgs = import (import ./default.nix).inputs.nixpkgs { }; in
pkgs.haskell.lib.buildStackProject {
  ghc = pkgs.haskell.compiler.ghc902;
  name = "hydra-tools";
  buildInputs = [ pkgs.zlib ];
}
