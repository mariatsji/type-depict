# nix-build nix/extrafiles.nix --arg artifact 'import ./default.nix' --argstr artifactName 'signature-visualizer' --arg extraDirs '[ ./webserver/assets ]'
{ artifact # artifact derivation to link in
, artifactName # artifact name
, extraDirs ? [ ] # array of nix-paths without trailing slash to join in
, nixpkgs ? import ./nixpkgs.nix 
}:

let
  # NB! Huge difference between [ builtins.toString ./. ] and [ "${./.}" ] - the last is in nix store and the first is relative!
  mkRelative = cd: dir:
    builtins.replaceStrings [ (builtins.toString cd) ] [ "" ]
    (builtins.toString dir);

  absolutes =
    nixpkgs.lib.flatten (map nixpkgs.lib.filesystem.listFilesRecursive extraDirs);
  copied =
    map (abs: nixpkgs.writeTextDir (mkRelative ./. abs) (builtins.readFile abs))
    absolutes;

in nixpkgs.symlinkJoin {
  name = artifactName + "-linked";
  paths = copied ++ [ artifact ];
  postBuild = "echo extrafiles linked";
}