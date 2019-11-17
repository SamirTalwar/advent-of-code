{ pkgs ? import <nixpkgs> {} }:

with pkgs;
let
  ghc = haskellPackages.ghcWithPackages (
    ps: with ps; [
      aeson
      array
      base
      bytestring
      containers
      cryptonite
      hashmap
      lens
      parsec
      scientific
      text
    ]
  );
in
stdenv.mkDerivation {
  name = "advent-of-code";

  buildInputs = [
    ghc
    swiProlog
  ];
}
