let nixpkgs = import ./nix/nixpkgs.nix;

in nixpkgs.haskellPackages.callCabal2nix "signature-visualizer" ./. {}