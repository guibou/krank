{ pkgs ? import ./nixpkgs.nix {} }:
with pkgs;
rec {
  inherit pkgs;

  # Explicit list of used files. Else there is always too much and
  # cache is invalidated.
  sources = lib.sourceByRegex ./.  [
    "krank\.cabal$"
    ".*\.hs$"
    "\.*.md$"
    "src"
    "app"
    "src/Krank"
    "src/Utils"
    "src/Krank/Checkers"
    "tests"
    "tests/Test"
    "tests/Test/Krank"
    "tests/Test/Krank/Checkers"
    "docs"
    "docs/Checkers"
    "LICENSE"
  ];

  # buildFromSdist ensures that the build will work on hackage
  # callCabal2nix behaves well with direnv
  krankBuilder = hPkgs: haskell.lib.buildFromSdist (hPkgs.callCabal2nix "krank" sources {});

  krank_86 = krankBuilder haskell.packages.ghc865;
  krank_88 = krankBuilder (haskell.packages.ghc881.override { overrides = self: super: {
    # RSA < 2.4 does not build with GHC 8.8
    RSA = super.RSA_2_4_1;
  };});

  # default is latest GHC
  krank = krank_88;

  # Run hlint on the codebase
  hlint = runCommand "hlint-krank" {
    nativeBuildInputs = [haskellPackages.hlint];
  }
  ''
  cd ${sources}
  hlint .
  mkdir $out
  '';

  # Run ormolu on the codebase
  # Fails if there is something to format
  ormolu = runCommand "ormolu-krank" {
    nativeBuildInputs = [haskellPackages.ormolu];
  }
  ''
  cd ${sources}
  ormolu --mode check $(find -name '*.hs')
  mkdir $out
  '';

  ormolu-fix = mkShell {
    nativeBuildInputs = [haskellPackages.ormolu git];
    shellHook = ''
      ormolu --mode inplace $(git ls-files | grep '\.hs$')
      exit 0
    '';
  };
}
