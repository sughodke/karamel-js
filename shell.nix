{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.ocaml
    pkgs.dune_3
    pkgs.ocamlPackages.findlib
    pkgs.ocamlPackages.ppx_deriving
    pkgs.ocamlPackages.ppx_deriving_yojson
    pkgs.ocamlPackages.zarith
    pkgs.ocamlPackages.pprint
    pkgs.ocamlPackages.menhir
    pkgs.ocamlPackages.menhirLib
    pkgs.ocamlPackages.sedlex
    pkgs.ocamlPackages.process
    pkgs.ocamlPackages.fix
    pkgs.ocamlPackages.wasm
    pkgs.ocamlPackages.visitors
    pkgs.ocamlPackages.uucp
  ];
}
