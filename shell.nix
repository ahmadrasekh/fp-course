{ nixpkgs ? import ./nix/sources.nix
}:
let
  helpers = import ./nix/helpers.nix;
  pkgs = import (nixpkgs.nixpkgs) { overlays = [ helpers ]; };
in
pkgs.mkShell {
  buildInputs = with pkgs.haskellPackages; [ ghc ghcid ];
}