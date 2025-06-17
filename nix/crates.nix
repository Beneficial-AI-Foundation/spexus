{ inputs, ... }:
{
  perSystem =
    {
      pkgs,
      ...
    }:
    {
      nci =
        let
          buildInputs = import ./buildInputs.nix { inherit pkgs; };
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
