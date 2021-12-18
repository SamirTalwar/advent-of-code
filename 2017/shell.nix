{ pkgs ? import <nixpkgs> { } }:
let
  swiProlog = import ../nix/swi-prolog { inherit pkgs; };
in
with pkgs;
stdenv.mkDerivation {
  name = "advent-of-code-2017";

  buildInputs = [
    swiProlog
  ];
}
