let
  sha256 = "1f2g3z9xm9h2im2zi246lz8dcd10l1f7qjlg9cri40s7q5x900iq";
  rev = "375fc4644cb4cb263233bde8b5b4eff1e692359b";
in
import (fetchTarball {
  inherit sha256;
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
})
