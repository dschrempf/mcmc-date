{
  description = "Date phylogenetic trees with Mcmc";

  inputs.dschrempf-nur.url = "github:dschrempf/nur-packages";
  inputs.dschrempf-nur.inputs.nixpkgs.follows = "nixpkgs";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  inputs.mcmc.url = "github:dschrempf/mcmc";
  inputs.mcmc.inputs.nixpkgs.follows = "nixpkgs";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, dschrempf-nur, flake-utils, mcmc, nixpkgs }:
    flake-utils.lib.eachDefaultSystem (
      system:
        let
          mcmc-overlay = (
            selfn: supern: {
              haskellPackages = supern.haskellPackages.override {
                overrides = selfh: superh:
                  {
                    mcmc = mcmc.defaultPackage.${system};
                  };
              };
            }
          );
          overlays = [ mcmc-overlay ];
          pkgs = import nixpkgs {
            inherit system overlays;
          };
          dschrempf = import dschrempf-nur {
            inherit pkgs;
          };
          mcmc-date-package =
            let
              p = pkgs.haskellPackages.callCabal2nix "mcmc-date" ./. rec {};
            in
              pkgs.haskell.lib.doBenchmark p;
        in
          {
            defaultPackage = mcmc-date-package;

            devShell = pkgs.haskellPackages.shellFor {
              shellHook =
                let
                  scripts = ./scripts;
                in
                  ''
                    export PATH="${scripts}:$PATH"
                  '';
              packages = _: [ mcmc-date-package ];
              buildInputs = [
                dschrempf.beast2
                dschrempf.figtree
                dschrempf.iqtree2
                dschrempf.phylobayes
                dschrempf.tracer
                pkgs.haskellPackages.elynx
                pkgs.haskellPackages.slynx
                pkgs.haskellPackages.tlynx
              ];
              doBenchmark = true;
            };
          }
    );
}
