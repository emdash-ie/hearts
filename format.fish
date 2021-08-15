#!/usr/bin/env fish
# Formats the haskell code with fourmolu
nix-shell --run 'find src -name "*.hs" -exec fourmolu -m inplace "{}" ";"'
nix-shell --run 'find app -name "*.hs" -exec fourmolu -m inplace "{}" ";"'
