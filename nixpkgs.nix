let
  sha256 = "1f6q98vx3sqxcn6qp5vpy00223r9hy93w9pxq65h9gdwzy3w4qxv";
  rev = "c6c4a3d45ab2";
in
import (fetchTarball {
  inherit sha256;
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
})
