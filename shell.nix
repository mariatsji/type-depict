{ pkgs ? import ./nix/nixpkgs.nix, herokuSecret ? null }:

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
  HEROKU_API_KEY = herokuSecret;
  shellHook =
    if isNull herokuSecret then "" else ''
      heroku container:login
      docker login --username=_ --password=$(heroku auth:token) registry.heroku.com
    '';
}
