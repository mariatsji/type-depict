{ artifact }:

let
  nixpkgs = import ./nixpkgs.nix;
  artifactName = "signature-visualizer";
  extraFiles = ../assets;
  
  # builds a base image to extend with a stack-built binary
in with nixpkgs; dockerTools.buildLayeredImage {
  name = artifactName;
  tag = "latest";
  created = "now";
  # runtime system deps  and binary tools in the docker image goes here
  contents = [
    artifact
    cacert
    libiconv
    libffi
    gmp
    busybox
    bash
    curl
    zlib
    tzdata
  ];

  extraCommands = ''
    cp -rf ${extraFiles} assets
  '';

  config = {
    Env = [
      "TZ=Europe/Oslo"
      "PATH=${bash}/bin:${busybox}/bin:${curl}/bin"
    ];
    Cmd = [ "${artifact}/bin/signature-visualizer-webserver" ];
  };
}