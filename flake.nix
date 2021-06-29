{
  description = "Evolution environment";

  inputs.nixpkgs.url = "path:/home/dominik/Nix/Nixpkgs";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.dschrempf-nur.url = "path:/home/dominik/Shared/config/nixos/nur";
  inputs.dschrempf-nur.inputs.nixpkgs.follows = "nixpkgs";

  outputs = { self, nixpkgs, flake-utils, dschrempf-nur }:
    flake-utils.lib.eachDefaultSystem (
      system:
        let
          pkgs = import nixpkgs {
            inherit system;
            config = { allowUnfree = true; };
          };
          dschrempf = import dschrempf-nur {
            inherit pkgs;
          };
          evolHaskellEnv = pkgs.haskell.packages.ghc8104.ghcWithPackages
            (
              haskellPackages: with haskellPackages; [
                # ELynx library.
                elynx
                slynx
                tlynx
              ]
            );
        in
          {
            devShell = pkgs.mkShell {
              shellHook = let
                scripts = ./scripts;
              in
                ''
                  export PATH="${scripts}:$PATH"
                '';
              packages =
                with dschrempf;
                [
                  # GHC with libraries.
                  evolHaskellEnv

                  # Tools.
                  beast2
                  figtree
                  iqtree2
                  phylobayes
                  tracer
                ];
            };
          }
    );
}
