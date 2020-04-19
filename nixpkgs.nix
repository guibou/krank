let
  # nixpkgs master of 17-04-2020 (after haskell 8.10 fix is merged)
  sha256 = "01z81ibvhv8fzy6f2j4z2ix6sdk73v796p63ndgrykxhn5ncv7wm";
  rev = "e9687df24a6219eeb01134b6cea2d45decc20024";
in
import (fetchTarball {
  inherit sha256;
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
})
