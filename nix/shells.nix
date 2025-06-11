{ inputs, ... }:
{
  perSystem =
    { pkgs, config, ... }:
    {
      devShells = {
        default =
          let
            name = "Proofstack-agnostic Copilot dev";
            shellHook = "echo ${name}";
            buildInputs = with pkgs; [
              elan
              uv
              cargo
              pnpm
              typescript
              lefthook
            ];
          in
          pkgs.mkShell { inherit name shellHook buildInputs; };
        spexus = config.nci.outputs.spexus.devShell;
      };
    };
}
