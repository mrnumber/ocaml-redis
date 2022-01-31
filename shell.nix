{ pkgs ? import <nixpkgs> {}
, ocamlVersion ? import ./nix/ocamlDefaultVersion.nix }:
let
  ocamlPackages = pkgs.ocaml-ng."ocamlPackages_${ocamlVersion}";
  local = pkgs.callPackage ./. { inherit ocamlVersion; };
in
pkgs.mkShell {
  inputsFrom = with local; [ redis redis-lwt redis-sync ];
  buildInputs = [ ocamlPackages.ocaml-lsp ocamlPackages.ocp-indent ] ++ local.testPackages;
}
