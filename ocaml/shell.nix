{ pkgs ? import ./nix }:
let
  inherit (pkgs) mkShell ocamlPackages fswatch opam;
in
pkgs.mkShell {
  buildInputs = with ocamlPackages; [
    alcotest
    base
    core
    dream
    dune_3
    findlib
    fmt
    fswatch
    merlin
    ocaml
    ocaml-lsp
    ocamlformat
    opam
    ounit2
    ppx_deriving
    ppx_inline_test
    ppx_show
    stdcompat
    utop
  ];
}
