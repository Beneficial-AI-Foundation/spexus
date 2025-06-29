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
  uv
  pnpm
  typescript
  lefthook
  claude-code
]
