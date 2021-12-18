{ pkgs ? import <nixpkgs> { } }:
let
  ghc = pkgs.haskellPackages.ghcWithPackages (
    ps: with ps; [
      array
      base
      bytestring
      comonad
      containers
      parsec
      split
      text
    ]
  );
in
with pkgs;
stdenv.mkDerivation {
  name = "advent-of-code-2021";

  buildInputs = [
    ghc
    haskell-language-server
    hlint
    ormolu
  ];
}
