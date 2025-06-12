{
  perSystem =
    { config, ... }:
    {
      packages = {
        default = config.packages.spexus-cli-release;
        mcp = config.packages.spexus-mcp-release;
      };
    };
}
