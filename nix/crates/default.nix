{ inputs, ... }:
{
  perSystem =
    {
      ...
    }:
    {
      nci = {
        projects.spexus = {
          path = inputs.self;
          export = true;
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
