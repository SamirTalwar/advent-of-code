{ pkgs ? import <nixpkgs> { } }:
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
