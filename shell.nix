with import ./. {};
krank.env.overrideAttrs (old: {
   # The haskell environment does not come with cabal-install
   nativeBuildInputs = old.nativeBuildInputs ++ [pkgs.cabal-install];
})
