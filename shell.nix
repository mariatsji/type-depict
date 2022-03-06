{ pkgs ? import ./nix/nixpkgs.nix }:

let haskellStuff = with pkgs;
        [ 
            haskellPackages.haskell-language-server
            ghc
            haskellPackages.cabal-install
            haskellPackages.cabal2nix
            haskellPackages.implicit-hie
            ghcid
            haskellPackages.fourmolu
        ];
    tools = with pkgs;
        [ 
            nixfmt
            git
            curl
            heroku
        ];
    all = haskellStuff ++ tools;


in pkgs.mkShell {
  # specify which packages to add to the shell environment
  packages = all;
  # add all the dependencies, of the given packages, to the shell environment
  inputsFrom = with pkgs; all;
}
