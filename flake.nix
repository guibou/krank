{
  description = "Krank";

  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nixpkgs.url = "github:nixos/nixpkgs";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};

      in
      rec {
        krankBuilder = hPkgs: pkgs.haskell.lib.buildFromSdist (hPkgs.callCabal2nix "krank" ./. {});
        defaultPackage = packages.krank;

        packages = {
           krank = krankBuilder pkgs.haskellPackages;
           #krank_88 = krankBuilder pkgs.haskell.packages.ghc88;
           krank_90 = krankBuilder pkgs.haskell.packages.ghc90;
           krank_92 = krankBuilder pkgs.haskell.packages.ghc92;
           krank_94 = krankBuilder pkgs.haskell.packages.ghc94;
           #krank_810 = krankBuilder pkgs.haskell.packages.ghc810;
        };

        all = pkgs.linkFarmFromDrvs "all" (builtins.attrValues packages);

        apps = {
          ormolu = {
            type = "app";
            program = "${pkgs.writeScript "ormolu" ''
              ${pkgs.ormolu}/bin/ormolu --mode inplace $(git ls-files | grep '\.hs$')
            ''}";
          };
        };
      });
}
