{
  nixpkgs ? builtins.fetchTarball {
    # rev = "e7d2f4f8dd9e31c6d6d232dfcc049248886d3ea0";
    # url = "https://github.com/NixOS/nixpkgs.git";
    url = "https://github.com/NixOS/nixpkgs/archive/refs/tags/20.09.tar.gz";
  },
  pkgs ? import nixpkgs {}
}:
let
  inherit (pkgs) lib haskell;
  reflexPlatform = import reflexPlatformSrc {};

  reflexPlatformSrc = builtins.fetchGit {
    url = "https://github.com/reflex-frp/reflex-platform.git";
    rev = "f019863c21ee85498e6a6e0072e617b2462b70ed";
  };

  extraPackages = super: {
    htmlt-presentation = ./.;
    htmlt = builtins.fetchGit {
      url = "https://github.com/lagunoff/htmlt.git";
      rev = "b37c48007af8977f4209a04cb8e6a4e3f5d8d7e3";
    };
  };

  mkPackages = self: super: super.override {
    overrides = lib.composeExtensions (self: super:
      lib.mapAttrs (k: v: self.callCabal2nix k v {}) (extraPackages super)
    ) (self: super: {
      htmlt = haskell.lib.dontHaddock super.htmlt;
    });
  };
in rec {
  inherit pkgs;
  ghcjs = mkPackages ghcjs reflexPlatform.ghcjs;
  ghc = mkPackages ghc pkgs.haskellPackages;
  shell.ghcjs = pkgs.mkShell {
    nativeBuildInputs = [pkgs.haskellPackages.cabal-install pkgs.inotify-tools];
    inputsFrom = [ghcjs.htmlt-presentation.env];
  };
}
