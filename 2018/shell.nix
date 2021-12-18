{ pkgs ? import <nixpkgs> { } }:
with pkgs;
stdenv.mkDerivation {
  name = "advent-of-code-2018";

  buildInputs = [
    swiftformat
  ];
}
