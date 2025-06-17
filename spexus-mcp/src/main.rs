mod server;

use rmcp::transport::stdio;
use std::error::Error;

/// MCP server for spexus tools
#[tokio::main]
async fn main() -> Result<(), Box<dyn Error>> {
    let server = server::create_server();

    // Create and run the server with STDIO transport
    server.run_server(stdio()).await?;

    Ok(())
}
