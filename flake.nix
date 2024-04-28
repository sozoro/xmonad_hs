{
  description = "xmonad.hs";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }: {

    xmonad_hs = ./xmonad.hs;

    devShell.x86_64-linux = with nixpkgs.legacyPackages.x86_64-linux;
      pkgs.mkShell {
        nativeBuildInputs = [
          (haskellPackages.ghcWithPackages (p: with p; [ xmonad xmonad-contrib ]))
        ];
        shellHook = ''
          exit 0
        '';
      };
  };
}
