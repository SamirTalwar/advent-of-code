{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/f366af7a1b3891d9370091ab03150d3a6ee138fa.tar.gz") { } }:
with pkgs;
stdenv.mkDerivation {
  name = "advent-of-code-2019";

  buildInputs = [
    cargo
    rls
    rustPlatform.rust.rustc
    rustPlatform.rustcSrc
    rustfmt
  ];
}
