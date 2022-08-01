#!/usr/bin/env fish
echo "> cabal2nix . > hearts.nix"
cabal2nix . > hearts.nix
and echo "> nix-shell --run \"cabal $argv\""
and nix-shell --run "cabal $argv"
