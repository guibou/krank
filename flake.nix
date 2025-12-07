{
  description = "Krank";

  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nixpkgs.url = "github:nixos/nixpkgs";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
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
              (pkgs.haskell.lib.dontCheck (hPkgs.callCabal2nix "krank" ./. { }))
            ).overrideAttrs
              (oldAttrs: {
                buildInputs = oldAttrs.buildInputs;
                passthru = oldAttrs.passthru // { inherit shell shell_hls; };
              });
            # Add the GHC version in the package name
          in
          pkg.overrideAttrs (old: { name = "krank-ghc${hPkgs.ghc.version}"; });
      in
      rec {
        packages = {
          default = krankBuilder pkgs.haskellPackages;
          krank_94 = krankBuilder pkgs.haskell.packages.ghc94;
          krank_96 = krankBuilder pkgs.haskell.packages.ghc96;
          krank_98 = krankBuilder pkgs.haskell.packages.ghc98;
          krank_910 = krankBuilder pkgs.haskell.packages.ghc910;
          krank_912 = krankBuilder pkgs.haskell.packages.ghc912;
        };

        devShells = rec {
          shell = packages.default.shell;
          shell_hls = packages.default.shell_hls;
          default = shell_hls;
        };
      });
}
