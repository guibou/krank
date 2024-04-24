{
  description = "Krank";

  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nixpkgs.url = "github:nixos/nixpkgs";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};

      in
      rec {
        krankBuilder = hPkgs:
          let
            shell = pkg.env.overrideAttrs (old: {
              nativeBuildInputs = old.nativeBuildInputs
                ++ [ pkgs.cabal-install ];
            });

            # Shell with haskell language server
            shell_hls = shell.overrideAttrs (old: {
              nativeBuildInputs = old.nativeBuildInputs
                ++ [ hPkgs.haskell-language-server ];
            });

            pkg = (
              (pkgs.haskell.lib.dontCheck (hPkgs.callCabal2nix "krank" ./. { }))).overrideAttrs
              (oldAttrs: {
                buildInputs = oldAttrs.buildInputs;
                passthru = oldAttrs.passthru // { inherit shell shell_hls; };
              });
            # Add the GHC version in the package name
          in pkg.overrideAttrs (old: { name = "krank-ghc${hPkgs.ghc.version}"; });

        defaultPackage = packages.krank;

        packages = {
           krank = krankBuilder pkgs.haskellPackages;
           #krank_88 = krankBuilder pkgs.haskell.packages.ghc88;
           krank_90 = krankBuilder pkgs.haskell.packages.ghc90;
           krank_92 = krankBuilder pkgs.haskell.packages.ghc92;
           krank_94 = krankBuilder pkgs.haskell.packages.ghc94;
           krank_96 = krankBuilder pkgs.haskell.packages.ghc96;
           krank_98 = krankBuilder pkgs.haskell.packages.ghc98;
           #krank_810 = krankBuilder pkgs.haskell.packages.ghc810;
        };

        devShell = packages.krank.shell_hls;
        devShells = {
          shell = packages.krank.shell;
          shell_hls = packages.krank.shell_hls;
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
