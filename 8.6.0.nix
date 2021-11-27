{
  nixpkgs ? builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/refs/tags/20.09.tar.gz";
  }
}:
let
  inherit (pkgs) lib haskell;
  pkgs = import nixpkgs {};
  reflexPlatform = import reflexPlatformSrc {};

  reflexPlatformSrc = builtins.fetchGit {
    url = "https://github.com/reflex-frp/reflex-platform.git";
    rev = "f019863c21ee85498e6a6e0072e617b2462b70ed";
  };

  extraPackages = super: {
    htmlt-presentation = ./.;
    htmlt = builtins.fetchGit {
      url = "https://github.com/lagunoff/htmlt.git";
      rev = "fd495db4d3b1e91e55f1389c5c2c84ddaf4d12d8";
    };
  };

  mkPackages = self: super: super.override {
    overrides = lib.composeExtensions (self: super:
      lib.mapAttrs (k: v: self.callCabal2nix k v {}) (extraPackages super)
    ) (self: super: {
      htmlt = haskell.lib.dontHaddock super.htmlt;
    });
  } // {
    shell = pkgs.mkShell {
      inputsFrom = [self.htmlt-presentation.env];
    };
  };
in rec {
  ghcjs = mkPackages ghcjs reflexPlatform.ghcjs;
}
