{ pkgs ? import <nixpkgs> { } }:
let
  ghc = pkgs.haskellPackages.ghcWithPackages (
    ps: with ps; [
      array
      base
      bytestring
      containers
      cryptonite
      parsec
      text
      unordered-containers
    ]
  );
in
with pkgs;
stdenv.mkDerivation {
  name = "advent-of-code-2016";

  buildInputs = [
    ghc
    haskell-language-server
    hlint
    ormolu
  ];
}
