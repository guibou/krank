{ nixpkgs ? ./nixpkgs.nix }:
with import nixpkgs {
  config = { allowBroken = true; };
};
rec {
  krank = haskellPackages.developPackage {
    name = "krank";

    # Filter temp files, git files, .ghc.environment and .nix files
    # which are not part of the build
    root = lib.sources.cleanSourceWith {
      filter = name: type: let baseName = baseNameOf (toString name); in
      !(lib.hasPrefix ".ghc.environment." baseName) && (baseName != "default.nix");
      src = lib.sources.cleanSource ./.;
    };

    overrides = self : super : {
    };
  };

  # Build krank from sdist file. This is useful to check
  # That the sdist contains the required files.
  krank-sdist = haskell.lib.buildFromSdist krank;
}
