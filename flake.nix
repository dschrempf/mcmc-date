{
  description = "Date phylogenetic trees with Mcmc";

  inputs.circular.url = "github:dschrempf/circular";

  inputs.covariance.url = "github:dschrempf/covariance";

  inputs.dirichlet.url = "github:dschrempf/dirichlet";

  inputs.dschrempf-nur.url = "github:dschrempf/nur-packages";

  inputs.elynx.url = "github:dschrempf/elynx";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  inputs.mcmc.url = "github:dschrempf/mcmc";
  # inputs.mcmc.url = "path:/home/dominik/Shared/haskell/mcmc";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable-small";
  # inputs.nixpkgs.url = "path:/home/dominik/Nix/Nixpkgs";

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
        ghcVersion = "ghc924";
        haskellOverlay = (
          selfn: supern: {
            haskellPackages = supern.haskell.packages.${ghcVersion}.override {
              overrides = selfh: superh:
                {
                  circular = circular.packages.${system}.default;
                  covariance = covariance.packages.${system}.default;
                  dirichlet = dirichlet.packages.${system}.default;
                  mcmc = mcmc.packages.${system}.default;
                  pava = pava.packages.${system}.default;
                } // elynx.packages.${system};
            };
          }
        );
        overlays = [ haskellOverlay ];
        pkgs = import nixpkgs {
          inherit system overlays;
        };
        hpkgs = pkgs.haskellPackages;
        dschrempf = import dschrempf-nur {
          inherit pkgs;
        };
        mcmcDatePackage =
          let
            p = hpkgs.callCabal2nix "mcmc-date" ./. rec { };
          in
          pkgs.haskell.lib.doBenchmark p;
      in
      {
        packages.default = mcmcDatePackage;

        devShells.default = hpkgs.shellFor {
          shellHook =
            let
              scripts = ./scripts;
            in
            ''
              export PATH="${scripts}:$PATH"
            '';
          packages = _: [ mcmcDatePackage ];
          nativeBuildInputs = with pkgs; [
            # Misc.
            bashInteractive

            # TODO: `cabal-fmt` fails to build when using a newer package set.
            haskell.packages.ghc902.cabal-fmt # Build fails for newer hpkgs.

            hpkgs.cabal-install
            hpkgs.haskell-language-server

            # Analysis.
            dschrempf.beast2
            dschrempf.figtree
            dschrempf.iqtree2
            dschrempf.phylobayes
            dschrempf.tracer

            # ELynx.
            hpkgs.elynx
            hpkgs.slynx
            hpkgs.tlynx

            # Profiling.
            hpkgs.eventlog2html
          ];
          buildInputs = with pkgs; [
          ];
          doBenchmark = true;
          # withHoogle = true;
        };
      }
    );
}
