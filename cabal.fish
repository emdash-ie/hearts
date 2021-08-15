#!/usr/bin/env fish
cabal2nix . > hearts.nix
and nix-shell --run "cabal $argv"
