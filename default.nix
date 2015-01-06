let pkgs = import <nixpkgs> {};
in pkgs.haskellPackages.callPackage ./threads.nix {}
