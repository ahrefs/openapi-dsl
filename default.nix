{ pkgs ? import <nixpkgs> { } }:

let
  ocamlPackages = pkgs.ocamlPackages;
  # Jsonkit is not yet packaged in nixpkgs.
  jsonkit = ocamlPackages.buildDunePackage {
    pname = "jsonkit";
    version = "1.1.0";
    src = pkgs.fetchFromGitHub {
      owner = "melange-community";
      repo = "jsonkit";
      rev = "jsonkit.1.1.0";
      hash = "sha256-sCf9K8/bmHDE/Bmwwgp641VjCPKMNQSkZ7D6+w0cbd4=";
    };
    propagatedBuildInputs = with ocamlPackages; [ ppxlib yojson ];
  };
in
ocamlPackages.buildDunePackage {
  pname = "openapi-dsl";
  version = "0.1.0";
  src = pkgs.lib.cleanSource ./.;
  propagatedBuildInputs = [ jsonkit ocamlPackages.yojson ];
}
