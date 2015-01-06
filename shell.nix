let
  pkgs = import <nixpkgs> {};
  haskellPackages = pkgs.haskellPackages.override {
    extension = self: super: {
      threads = self.callPackage ./threads.nix {};
    };
  };

in pkgs.myEnvFun {
     name = haskellPackages.threads.name;
     buildInputs = [
       (haskellPackages.ghcWithPackages (hs: ([
         hs.cabalInstall
       ] ++ hs.threads.propagatedNativeBuildInputs
         ++ hs.threads.extraBuildInputs)))
     ];
   }
