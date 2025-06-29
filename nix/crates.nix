{ inputs, ... }:
{
  perSystem =
    { system, ... }:
    {
      nci =
        let
          buildInputs = import ./buildInputs.nix {
            inherit (inputs) nixpkgs;
            inherit system;
          };
        in
        {
          projects.spexus = {
            path = inputs.self;
            export = true;
            depsDrvConfig.mkDerivation = { inherit buildInputs; };
          };
          crates = {
            spexus-lang = { };
            spexus-cli = { };
            spexus-mcp = { };
            backends-common = { };
            spexus-kani = { };
            spexus-refinedc = { };
          };
        };
    };
}
