let pkgs = import ./nix/nixpkgs.nix;
in pkgs.haskellPackages.callCabal2nix "signature-visualizer" ./. {}
