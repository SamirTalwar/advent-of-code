{ pkgs ? import <nixpkgs> { }
, name
, src
,
}:
pkgs.stdenv.mkDerivation {
  name = "swiProlog-pack-" + name;

  src = src;

  buildInputs = [ pkgs.swiProlog ];

  buildPhase = ''
    echo ":- pack_install('file://$src', [package_directory('$out'), silent(true), interactive(false)]), halt." > install_pack.pl
    echo ":- halt(1)." >> install_pack.pl
  '';

  installPhase = ''
    mkdir $out
    swipl install_pack.pl
  '';
}
