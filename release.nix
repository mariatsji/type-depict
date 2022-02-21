let pkgs = import ./nixpkgs.nix;
    sim = pkgs.haskellPackages.callCabal2nix "sim" ./. {};

in pkgs.haskell.lib.overrideCabal sim (old: {
  enableSharedExecutables = false;
  enableSharedLibraries = false;
  configureFlags = [ ];
})