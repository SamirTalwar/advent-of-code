{ pkgs ? import <nixpkgs> { } }:
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
