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
            buildInputs = import ./buildInputs.nix { inherit pkgs; };
          in
          pkgs.mkShell { inherit name shellHook buildInputs; };
        spexus = config.nci.outputs.spexus.devShell;
      };
    };
}
