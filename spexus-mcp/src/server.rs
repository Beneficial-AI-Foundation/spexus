use rmcp::{
    ServerHandler, ServiceExt,
    model::{ServerCapabilities, ServerInfo},
    schemars, tool,
};
use spexus_kani::translate::translate_to_kani;
use spexus_lang::parser::SpexusParser;

#[derive(Debug, rmcp::serde::Deserialize, schemars::JsonSchema)]
pub struct TranslationRequest {
    #[schemars(description = "The spexus string to translate")]
    pub spexus_string: String,
}

#[derive(Debug, Clone)]
pub struct SpexusServer;

#[tool(tool_box)]
impl SpexusServer {
    pub fn new() -> Self {
        Self
    }

    #[tool(
        name = "translate_to_kani",
        description = "Translate a spexus string into a kani proof harness"
    )]
    fn translate_to_kani(
        &self,
        #[tool(aggr)] TranslationRequest { spexus_string }: TranslationRequest,
    ) -> String {
        // Parse the spexus string into a Spexus AST
        let spexus =
            SpexusParser::parse_spexus(&spexus_string).expect("Failed to parse spexus string");

        // Get the first spec entry and translate it
        let spec = spexus
            .entries
            .first()
            .expect("No spec entries found")
            .core();

        // Translate the spec into a kani proof harness
        translate_to_kani(spec)
    }

    #[tool(
        name = "translate_to_refinedc",
        description = "Translate a spexus string into a refinedc proof harness"
    )]
    fn translate_to_refinedc(
        &self,
        #[tool(aggr)] TranslationRequest { spexus_string }: TranslationRequest,
    ) -> String {
        // TODO: Implement actual refinedc translation logic
        format!("// Generated refinedc annotations for: {}", spexus_string)
    }

    #[tool(
        name = "validate",
        description = "Validate a spexus string and return empty string if valid, error message if invalid"
    )]
    fn validate(
        &self,
        #[tool(aggr)] TranslationRequest { spexus_string }: TranslationRequest,
    ) -> String {
        match SpexusParser::parse_spexus(&spexus_string) {
            Ok(_) => String::new(),
            Err(e) => e.to_string(),
        }
    }
}

#[tool(tool_box)]
impl ServerHandler for SpexusServer {
    fn get_info(&self) -> ServerInfo {
        ServerInfo {
            instructions: Some(
                "A server that provides tools for working with spexus strings, including translation to kani/refinedc and validation".into(),
            ),
            capabilities: ServerCapabilities::builder().enable_tools().build(),
            ..Default::default()
        }
    }
}

impl SpexusServer {
    pub async fn run_server(
        self,
        transport: (tokio::io::Stdin, tokio::io::Stdout),
    ) -> Result<(), std::io::Error> {
        let service = self.serve(transport).await?;
        service.waiting().await?;
        Ok(())
    }
}

pub fn create_server() -> SpexusServer {
    SpexusServer::new()
}
