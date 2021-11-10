{
  description = "Date phylogenetic trees with Mcmc";

  inputs.circular.url = "github:dschrempf/circular";

  inputs.covariance.url = "github:dschrempf/covariance";

  inputs.dirichlet.url = "github:dschrempf/dirichlet";

  inputs.dschrempf-nur.url = "github:dschrempf/nur-packages";

  inputs.elynx.url = "github:dschrempf/elynx";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  inputs.mcmc.url = "github:dschrempf/mcmc";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  inputs.pava.url = "github:dschrempf/pava";

  outputs =
    { self
    , circular
    , covariance
    , dirichlet
    , dschrempf-nur
    , elynx
    , flake-utils
    , mcmc
    , nixpkgs
    , pava
    }:
      flake-utils.lib.eachDefaultSystem (
        system:
          let
            haskell-overlay = (
              selfn: supern: {
                haskellPackages = supern.haskellPackages.override {
                  overrides = selfh: superh:
                    {
                      circular = circular.defaultPackage.${system};
                      covariance = covariance.defaultPackage.${system};
                      dirichlet = dirichlet.defaultPackage.${system};
                      mcmc = mcmc.defaultPackage.${system};
                      pava = pava.defaultPackage.${system};
                    } // elynx.packages.${system};
                };
              }
            );
            overlays = [ haskell-overlay ];
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
                buildInputs = with pkgs; [
                  bashInteractive
                  dschrempf.beast2
                  dschrempf.figtree
                  dschrempf.iqtree2
                  dschrempf.phylobayes
                  dschrempf.tracer
                  haskellPackages.elynx
                  haskellPackages.slynx
                  haskellPackages.tlynx

                  haskellPackages.cabal-install
                  haskellPackages.haskell-language-server
                  haskellPackages.stack
                ];
                doBenchmark = true;
                withHoogle = true;
              };
            }
      );
}
