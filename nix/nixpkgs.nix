let
    pkgs = import (builtins.fetchTarball {
        url = "https://github.com/NixOS/nixpkgs/archive/6c309ef9ae51fd4ebdcb3b19ab6acea5ba66983e.tar.gz";
    }) {};

in pkgs