{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/28aec35166022d8ecde7d210912179ab226e2397.tar.gz") { } }:
let
  ghc = pkgs.haskellPackages.ghcWithPackages (
    ps: with ps; [
      base
      containers
      megaparsec
    ]
  );
in
with pkgs;
stdenv.mkDerivation {
  name = "advent-of-code-2023";

  buildInputs = [
    ghc
    haskell-language-server
    hlint
    ormolu
  ];
}
