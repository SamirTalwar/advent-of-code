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
      split
      text
      unordered-containers
    ]
  );
  swiProlog = import ./nix/swi-prolog { inherit pkgs; };
in
with pkgs;
stdenv.mkDerivation {
  name = "advent-of-code";

  buildInputs = [
    # Rust
    cargo
    rls
    rustPlatform.rust.rustc
    rustPlatform.rustcSrc
    rustfmt

    # Haskell
    ghc
    haskell-language-server
    hlint
    ormolu

    # Pony
    pony-corral
    ponyc
    pcre2 # used by the Pony regex library

    # Prolog
    swiProlog

    # Swift
    swiftformat
  ];
}
