let nixpkgs = import ./nixpkgs.nix;

in nixpkgs.haskellPackages.callCabal2nix "signature-visualizer" ./. {}