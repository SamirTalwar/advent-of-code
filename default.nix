{ pkgs ? import <nixpkgs> { } }:

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
    cargo
    ghc
    python3
    rls
    rustPlatform.rust.rustc
    rustPlatform.rustcSrc
    rustfmt
    swiProlog
    swiftformat
  ];
}
