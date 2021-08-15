let
  pkgs = import <nixpkgs> { };

in
  { hearts = pkgs.haskellPackages.callPackage ./hearts.nix { };
  }
