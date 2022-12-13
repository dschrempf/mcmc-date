{
  description = "Date phylogenetic trees with Mcmc";

  inputs.circular.url = "github:dschrempf/circular";
  inputs.circular.inputs.flake-utils.follows = "flake-utils";
  inputs.circular.inputs.nixpkgs.follows = "nixpkgs";

  inputs.covariance.url = "github:dschrempf/covariance";
  inputs.covariance.inputs.flake-utils.follows = "flake-utils";
  inputs.covariance.inputs.nixpkgs.follows = "nixpkgs";

  inputs.dirichlet.url = "github:dschrempf/dirichlet";
  inputs.dirichlet.inputs.flake-utils.follows = "flake-utils";
  inputs.dirichlet.inputs.nixpkgs.follows = "nixpkgs";

  inputs.dschrempf-nur.url = "github:dschrempf/nur-packages";
  inputs.dschrempf-nur.inputs.flake-utils.follows = "flake-utils";
  inputs.dschrempf-nur.inputs.nixpkgs.follows = "nixpkgs";

  inputs.elynx.url = "github:dschrempf/elynx";
  inputs.elynx.inputs.flake-utils.follows = "flake-utils";
  inputs.elynx.inputs.nixpkgs.follows = "nixpkgs";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  inputs.mcmc.url = "github:dschrempf/mcmc";
  # inputs.mcmc.url = "path:/home/dominik/Shared/haskell/mcmc";
  inputs.mcmc.inputs.circular.follows = "circular";
  inputs.mcmc.inputs.covariance.follows = "covariance";
  inputs.mcmc.inputs.dirichlet.follows = "dirichlet";
  inputs.mcmc.inputs.flake-utils.follows = "flake-utils";
  inputs.mcmc.inputs.nixpkgs.follows = "nixpkgs";
  inputs.mcmc.inputs.pava.follows = "pava";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable-small";
  # inputs.nixpkgs.url = "path:/home/dominik/Nix/Nixpkgs";

  inputs.pava.url = "github:dschrempf/pava";
  inputs.pava.inputs.flake-utils.follows = "flake-utils";
  inputs.pava.inputs.nixpkgs.follows = "nixpkgs";

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
    let
      theseHpkgNames = [
        "mcmc-date"
      ];
      thisGhcVersion = "ghc943";
      hOverlay = selfn: supern: {
        haskell = supern.haskell // {
          packageOverrides = selfh: superh:
            supern.haskell.packageOverrides selfh superh //
              {
                mcmc-date = selfh.callCabal2nix "mcmc-date" ./. { };
              };
        };
      };
      perSystem = system:
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [
              hOverlay
              circular.overlays.default
              covariance.overlays.default
              dirichlet.overlays.default
              mcmc.overlays.default
              pava.overlays.default
              elynx.overlays.default
            ];
          };
          hpkgs = pkgs.haskell.packages.${thisGhcVersion};
          hlib = pkgs.haskell.lib;
          theseHpkgs = nixpkgs.lib.genAttrs theseHpkgNames (n: hpkgs.${n});
          theseHpkgsDev = builtins.mapAttrs (_: x: hlib.doBenchmark x) theseHpkgs;
          dschrempf = import dschrempf-nur {
            inherit pkgs;
            inherit system;
          };
        in
        {
          packages = theseHpkgs // { default = theseHpkgs.mcmc-date; };

          devShells.default = hpkgs.shellFor {
            shellHook =
              let
                scripts = ./scripts;
              in
              ''
                export PATH="${scripts}:$PATH"
              '';
            packages = _: (builtins.attrValues theseHpkgsDev);
            nativeBuildInputs = with pkgs; [
              # See https://github.com/NixOS/nixpkgs/issues/59209.
              bashInteractive

              hpkgs.cabal-fmt
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
              # TODO (broken dependency, 2022-12-13): hpkgs.eventlog2html
            ];
            buildInputs = with pkgs; [
            ];
            doBenchmark = true;
            # withHoogle = true;
          };
        };
    in
    { overlays.default = hOverlay; } // flake-utils.lib.eachDefaultSystem perSystem;
}
