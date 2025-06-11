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
          buildInputs = import "${inputs.self}/nix/buildInputs.nix" { inherit pkgs; };
        in
        {
          projects.spexus = {
            path = inputs.self;
            export = true;
            depsDrvConfig.mkDerivation = { inherit buildInputs; };
          };
          crates = {
            lang = { };
            cli = { };
            mcp = { };
            backend-common = { };
            backend-kani = { };
            backend-refinedc = { };
          };
        };
    };
}
