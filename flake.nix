{
  description = "Date phylogenetic trees with Mcmc";

  inputs.dschrempf-nur.url = "github:dschrempf/nur-packages";
  inputs.dschrempf-nur.inputs.flake-utils.follows = "flake-utils";
  inputs.dschrempf-nur.inputs.nixpkgs.follows = "nixpkgs";

  inputs.elynx.url = "github:dschrempf/elynx";
  inputs.elynx.inputs.flake-utils.follows = "flake-utils";
  inputs.elynx.inputs.nixpkgs.follows = "nixpkgs";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  inputs.mcmc.url = "github:dschrempf/mcmc";
  # inputs.mcmc.url = "path:/home/dominik/Shared/haskell/mcmc";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  # inputs.nixpkgs.url = "path:/home/dominik/Nix/Nixpkgs";

  outputs =
    { self
    , dschrempf-nur
    , elynx
    , flake-utils
    , mcmc
    , nixpkgs
    }:
    let
      theseHpkgNames = [
        "mcmc-date"
      ];
      thisGhcVersion = "ghc96";
      hOverlay = selfn: supern: {
        haskell = supern.haskell // {
          packageOverrides = selfh: superh:
            supern.haskell.packageOverrides selfh superh //
              {
                mcmc-date = selfh.callCabal2nix "mcmc-date" ./. { };
              };
        };
      };
      overlays = [
        hOverlay
        mcmc.overlays.default
        elynx.overlays.default
      ];
      perSystem = system:
        let
          pkgs = import nixpkgs {
            inherit system;
            inherit overlays;
          };
          hpkgs = pkgs.haskell.packages.${thisGhcVersion};
          hlib = pkgs.haskell.lib;
          theseHpkgs = nixpkgs.lib.genAttrs theseHpkgNames (n: hpkgs.${n});
          theseHpkgsDev = builtins.mapAttrs (_: x: hlib.doBenchmark x) theseHpkgs;
          dschrempf = dschrempf-nur.packages.${system};
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
              # Haskell toolchain.
              hpkgs.cabal-fmt
              hpkgs.cabal-install
              hpkgs.haskell-language-server

              # Analysis.
              dschrempf.beast2
              dschrempf.fasttree
              dschrempf.figtree
              dschrempf.iqtree2
              dschrempf.phylobayes
              dschrempf.tracer

              # ELynx.
              hpkgs.elynx
              hpkgs.slynx
              hpkgs.tlynx
              gnuplot

              # Profiling.
              hpkgs.eventlog2html
            ];
            buildInputs = with pkgs; [
            ];
            doBenchmark = true;
            # withHoogle = true;
          };
        };
    in
    { overlays.default = nixpkgs.lib.composeManyExtensions overlays; }
    // flake-utils.lib.eachDefaultSystem perSystem;
}
