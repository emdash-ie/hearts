let
  pkgs = import <nixpkgs> { };
in pkgs.mkShell {
  packages = [ pkgs.haskellPackages.fourmolu ];
  inputsFrom = [ (import ./release.nix).hearts.env ];
}
