# hell - a shell utility written in haskell

## usage

    h --help

## development

vscode + nix-environment selector plugin

## test

    nix-shell --run 'cabal test'

    nix-build release.nix

## build

    nix-shell --run 'cabal build'

    nix-build release.nix

## use

put this in your .zshrc

    STACKLOCALS=~/.local/bin
    CABALLOCALS=~/.cabal/bin
    export PATH=$PATH:$STACKLOCALS:$CABALLOCALS

    alias docker='podman'

    alias s='hell s'
    alias d='hell d'
    alias p='hell p'

