{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/4633a7c72337ea8fd23a4f2ba3972865e3ec685d.tar.gz") { } }:
with pkgs;
stdenv.mkDerivation {
  name = "advent-of-code-2024";

  buildInputs = [
    dotnetCorePackages.sdk_9_0
    omnisharp-roslyn
  ];
}
