{ pkgs ? import <nixpkgs> { } }:

let
  regex = import ./pack.nix {
    inherit pkgs;
    name = "regex";
    src = pkgs.fetchFromGitHub {
      owner = "mndrix";
      repo = "regex";
      rev = "v0.3.3";
      sha256 = "03plkwa2phwdjp8hv36a3a98q25j1ks42lzgz0dnvxladjx9pbi1";
    };
  };

  packs = [
    regex
  ];

  loadPacks = pkgs.writeTextFile {
    name = "load_packs.pl";
    text = pkgs.lib.strings.concatMapStrings (s: s + "\n") (
      [":-"] ++
      (pkgs.lib.lists.map (pack: "  asserta( (file_search_path(pack, '${pack}')) ),") packs) ++
      ["  attach_packs."]
    );
  };
in
pkgs.writeScriptBin "swipl" ''
  #!/usr/bin/env bash
  exec ${pkgs.swiProlog}/bin/swipl -s ${loadPacks} "$@"
''
