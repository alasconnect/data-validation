{ compiler ? "ghc865" }:

# allows running an interactive shell for testing
# can be invoked like `nix-shell -I nixpkgs=https://github.com/nixos/nixpkgs-channels/archive/nixos-19.09.tar.gz`

let
  nixpkgs = import <nixpkgs> {};
  inherit (nixpkgs) pkgs stdenv;
in
# Make a new "derivation" that represents our shell
stdenv.mkDerivation {
  name = "data-validation";

  # The packages in the `buildInputs` list will be added to the PATH in our shell
  buildInputs = [
    pkgs.cabal-install
    pkgs.haskell.compiler.${compiler}
    pkgs.haskellPackages.hspec-discover
  ];

}
