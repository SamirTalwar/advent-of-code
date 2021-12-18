{ pkgs ? import <nixpkgs> { } }:
with pkgs;
stdenv.mkDerivation {
  name = "advent-of-code-2020";

  buildInputs = [
    pony-corral
    ponyc
    pcre2 # used by the Pony regex library
  ];
}
