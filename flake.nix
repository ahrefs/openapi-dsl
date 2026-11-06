{
  description = "An OCaml DSL for constructing OpenAPI documents";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        package = pkgs.callPackage ./default.nix { };
      in
      {
        packages = {
          default = package;
          openapi-dsl = package;
        };

        devShells.default = pkgs.mkShell {
          inputsFrom = [ package ];
          packages = with pkgs.ocamlPackages; [ ocaml dune_3 findlib ];
        };
      });
}
