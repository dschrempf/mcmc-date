{
  description = "ELynx environment";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
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
        in
          {
            devShell = pkgs.mkShell {
              nativeBuildInputs = [];
              buildInputs = with pkgs.haskell.packages.ghc8104;
                [
                  elynx
                  slynx
                  tlynx
                  dschrempf.phylobayes
                ];
            };
          }
    );
}
