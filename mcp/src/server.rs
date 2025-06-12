use rmcp::{
    RoleServer, ServerHandler, ServiceExt,
    model::{ServerCapabilities, ServerInfo},
    schemars, tool,
};
use std::error::Error;

#[derive(Debug, rmcp::serde::Deserialize, schemars::JsonSchema)]
pub struct TranslationRequest {
    #[schemars(description = "The spexus string to translate")]
    pub spexus_string: String,
}

#[derive(Debug, Clone)]
pub struct KaniServer;

#[tool(tool_box)]
impl KaniServer {
    pub fn new() -> Self {
        Self
    }

    #[tool(description = "Translate a spexus string into a kani proof harness")]
    fn translate(
        &self,
        #[tool(aggr)] TranslationRequest { spexus_string }: TranslationRequest,
    ) -> String {
        // TODO: Implement actual kani translation logic
        format!("// Generated kani proof harness for: {}", spexus_string)
    }
}

#[tool(tool_box)]
impl ServerHandler for KaniServer {
    fn get_info(&self) -> ServerInfo {
        ServerInfo {
            instructions: Some(
                "A server that translates spexus strings into kani proof harnesses".into(),
            ),
            capabilities: ServerCapabilities::builder().enable_tools().build(),
            ..Default::default()
        }
    }
}

#[derive(Debug, Clone)]
pub struct RefinedCServer;

#[tool(tool_box)]
impl RefinedCServer {
    pub fn new() -> Self {
        Self
    }

    #[tool(description = "Translate a spexus string into a refinedc proof harness")]
    fn translate(
        &self,
        #[tool(aggr)] TranslationRequest { spexus_string }: TranslationRequest,
    ) -> String {
        // TODO: Implement actual refinedc translation logic
        format!("// Generated refinedc proof harness for: {}", spexus_string)
    }
}

#[tool(tool_box)]
impl ServerHandler for RefinedCServer {
    fn get_info(&self) -> ServerInfo {
        ServerInfo {
            instructions: Some(
                "A server that translates spexus strings into refinedc proof harnesses".into(),
            ),
            capabilities: ServerCapabilities::builder().enable_tools().build(),
            ..Default::default()
        }
    }
}

pub enum Server {
    Kani(KaniServer),
    RefinedC(RefinedCServer),
}

impl Server {
    pub async fn serve(
        self,
        transport: (tokio::io::Stdin, tokio::io::Stdout),
    ) -> Result<(), std::io::Error> {
        match self {
            Server::Kani(server) => {
                let service = server.serve(transport).await?;
                service.waiting().await?;
                Ok(())
            }
            Server::RefinedC(server) => {
                let service = server.serve(transport).await?;
                service.waiting().await?;
                Ok(())
            }
        }
    }
}

pub fn create_server(backend_name: &str) -> Result<Server, Box<dyn Error>> {
    match backend_name {
        "kani" => Ok(Server::Kani(KaniServer::new())),
        "refinedc" => Ok(Server::RefinedC(RefinedCServer::new())),
        _ => Err(format!("Unknown backend: {}", backend_name).into()),
    }
}
