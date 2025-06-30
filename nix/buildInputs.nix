{ nixpkgs, system }:
let
  pkgs = import nixpkgs {
    config.allowUnfree = true;
    inherit system;
  };
in
with pkgs;
[
  elan
  lefthook
  claude-code
  dafny
  cargo
]
