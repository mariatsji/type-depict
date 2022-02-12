let pkgs = import ./nixpkgs.nix;
    hell = pkgs.haskellPackages.callCabal2nix "hell" ./. {};

in pkgs.haskell.lib.overrideCabal hell (old: {
  enableSharedExecutables = false;
  enableSharedLibraries = false;
  configureFlags = [ ];
})