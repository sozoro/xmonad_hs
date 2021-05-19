{ pkgs ? import <nixpkgs> {}, compiler ? null }:
let
  haskellPackages = (if   isNull compiler
                     then pkgs.haskellPackages
                     else pkgs.haskell.packages.${compiler}).override {
    overrides = self: super: {
    };
  };
in
  pkgs.mkShell {
    nativeBuildInputs = [
      (haskellPackages.ghcWithPackages (p: with p; [ xmonad xmonad-contrib ]))
    ];
    shellHook = ''
      ghci xmonad.hs
      exit 0
    '';
  }
