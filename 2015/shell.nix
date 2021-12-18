{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/f366af7a1b3891d9370091ab03150d3a6ee138fa.tar.gz") { } }:
let
  ghc = pkgs.haskellPackages.ghcWithPackages (
    ps: with ps; [
      aeson
      array
      base
      bytestring
      containers
      cryptonite
      lens
      parsec
      scientific
      text
      unordered-containers
    ]
  );
in
with pkgs;
stdenv.mkDerivation {
  name = "advent-of-code-2015";

  buildInputs = [
    ghc
    haskell-language-server
    hlint
    ormolu
  ];
}
