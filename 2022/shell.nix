{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/227de2b3bbec142f912c09d5e8a1b4e778aa54fb.tar.gz") { } }:
with pkgs;
stdenv.mkDerivation {
  name = "advent-of-code-2022";

  buildInputs = [
    docker-compose
  ];
}
