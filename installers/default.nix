with (import (fetchTarball https://github.com/NixOS/nixpkgs/archive/56ebd9129956339ab98ba2f40cb233df0735f5a1.tar.gz) { config = {}; });

with haskell.lib;

let

  drv = justStaticExecutables (haskell.packages.ghc802.callPackage ./cardano-installer.nix {});

in

  if pkgs.lib.inNixShell then drv.env else drv

