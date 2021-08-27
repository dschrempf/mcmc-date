{ghc}:

with (import <nixpkgs> {});

haskell.lib.buildStackProject {
  inherit ghc;
  name = "stackEnv";
  buildInputs = [
    gfortran
    glpk
    gsl
    openblas
    pkg-config
    sundials
    zlib
  ];
}
