mod server;

use clap::{Parser, ValueEnum};
use rmcp::transport::stdio;
use std::error::Error;
use std::fmt;

/// MCP server for verification backends
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// The verification backend to use
    #[arg(value_enum)]
    backend: Backend,
}

#[derive(ValueEnum, Debug, Clone)]
enum Backend {
    Kani,
    Refinedc,
}

impl fmt::Display for Backend {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Backend::Kani => write!(f, "kani"),
            Backend::Refinedc => write!(f, "refinedc"),
        }
    }
}

// Run the server
#[tokio::main]
async fn main() -> Result<(), Box<dyn Error>> {
    let args = Args::parse();

    let server = server::create_server(&args.backend.to_string())?;

    // Create and run the server with STDIO transport
    server.serve(stdio()).await?;

    Ok(())
}
