% vim: set syntax=prolog

% To install, run:
% swipl -t install_packs prolog/packs.pl

load_packs :-
  absolute_file_name('prolog/packs', PacksDirectory),
  asserta( (file_search_path(pack, PacksDirectory)) ),
  attach_packs.

install_packs :-
  load_packs,
  pack_install(regex, [upgrade(true), interactive(false)]),
  halt.

:- load_packs.
