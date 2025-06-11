{
  perSystem =
    { config, ... }:
    {
      packages = {
        default = config.packages.cli-release;
        mcp = config.packages.mcp-release;
      };
    };
}
