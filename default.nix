let
  nixpkgs = import ./nix/nixpkgs.nix;

in nixpkgs.haskell.lib.justStaticExecutables
(nixpkgs.haskell.lib.disableLibraryProfiling (nixpkgs.haskell.lib.dontHaddock
  (nixpkgs.haskellPackages.callCabal2nix "signature-visualizer" ./. { })))
