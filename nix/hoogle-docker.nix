let
  system = "x86_64-linux";
  nixpkgs = import (builtins.fetchTarball {
        url = "https://github.com/NixOS/nixpkgs/archive/6c309ef9ae51fd4ebdcb3b19ab6acea5ba66983e.tar.gz";
    }) { inherit system; };
  artifact = with nixpkgs; haskell.packages.ghc8107.ghcWithPackages (pkgs: with pkgs; [ hoogle hoogle-index ]) ;
  
  # builds a base image to extend with a stack-built binary
in with nixpkgs; dockerTools.buildLayeredImage {
  name = "hoogle-docker";
  tag = "latest";
  created = "now";
  # runtime system deps  and binary tools in the docker image goes here
  # contents =  [
  #  hello
  # ];

  config = {
    Env = [
      "TZ=Europe/Oslo"
      "PATH=${bash}/bin:${busybox}/bin:${curl}/bin"
    ];
    # Cmd = [ "${artifact}/bin/signature-visualizer-webserver" ];
  };
}