let pkgs = import ./nixpkgs.nix;
    hev = pkgs.haskellPackages.callCabal2nix "expression-visualizer" ./. {};

in pkgs.haskell.lib.overrideCabal hev (old: {
  enableSharedExecutables = false;
  enableSharedLibraries = false;
  configureFlags = [ ];
})