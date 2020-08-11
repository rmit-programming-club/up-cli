{ pkgs ? import <nixpkgs> {}}:
pkgs.haskellPackages.callPackage ./nix/up-cli.nix {}
