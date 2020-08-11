{ pkgs ? import <nixpkgs> {} }:
let
  project = import ./. {};
in
pkgs.mkShell {
  shellHook = ''
#    export NIX_GHC="$(which ghc)"
#    export NIX_GHCPKGS="$(which ghc-pkg)"
    export NIX_GHC_LIBDIR="$(ghc --print-libdir)"
  '';
  name="up-cli-env";
  buildInputs = project.env.nativeBuildInputs ++ (with pkgs; [ cabal2nix haskellPackages.ghcide cabal-install zlib ]);
}
