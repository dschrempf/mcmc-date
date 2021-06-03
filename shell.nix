{ghc}:

with (import <nixpkgs> {config.allowUnfree = true;});

haskell.lib.buildStackProject {
  inherit ghc;
  name = "stackEnv";
  buildInputs = [
    gfortran
    glpk
    gsl
    mkl
    pkg-config
    sundials
    zlib
  ];
}
