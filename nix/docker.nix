{ artifact }:

let
  nixpkgs = import ./nixpkgs.nix;
  artifactName = "signaturevisualizer";

  nonRootShadowSetup = { user, uid, gid ? uid }: [
    (nixpkgs.writeTextDir "etc/shadow" ''
      root:!x:::::::
      ${user}:!:::::::
    '')
    (nixpkgs.writeTextDir "etc/passwd" ''
      root:x:0:0::/root:${nixpkgs.runtimeShell}
      ${user}:x:${toString uid}:${toString gid}::/home/${user}:
    '')
    (nixpkgs.writeTextDir "etc/group" ''
      root:x:0:
      ${user}:x:${toString gid}:
    '')
    (nixpkgs.writeTextDir "etc/gshadow" ''
      root:x::
      ${user}:x::
    '')
  ];

  processUser = "simon";
  artifactWithLinks = (import ./extrafiles.nix) { inherit artifact artifactName; extraDirs = [ ../webserver/assets ]; };

  # builds a base image to extend with a stack-built binary
in with nixpkgs; dockerTools.buildLayeredImage {
  name = artifactName;
  tag = "latest";
  created = "now";
  # runtime system deps  and binary tools in the docker image goes here
  contents = [
    artifactWithLinks
    libiconv
    libffi
    gmp
    busybox
    bash
    curl
    zlib
    tzdata
  ] ++ (nonRootShadowSetup {
    uid = 999;
    user = "${processUser}";
  });

  config = {
    Env = [
      "TZ=Europe/Oslo"
      "PATH=${bash}/bin:${busybox}/bin:${curl}/bin"
    ];
    WorkingDir = "${artifactWithLinks}";
    User = "${processUser}";
    Entrypoint = [ "${artifactWithLinks}/bin/signature-visualizer-webserver" ];
    ExposedPorts = { "3000/tcp" = { }; };
  };
}