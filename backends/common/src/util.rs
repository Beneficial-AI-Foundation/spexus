use lang::parser::SpexusParser;
use std::fs::File;
use std::io::{Error, Read};
use std::path::Path;

#[derive(Debug)]
pub enum SpexusError {
    IoError(Error),
    ParseError(String),
}

impl From<Error> for SpexusError {
    fn from(err: Error) -> Self {
        SpexusError::IoError(err)
    }
}

/// Parse a spexus string into a Spexus AST
pub fn parse_spexus_string(content: &str) -> Result<lang::syntax::Spexus, SpexusError> {
    SpexusParser::parse_spexus(content).map_err(|e| SpexusError::ParseError(e.to_string()))
}

/// Reads and parses a .spexus source file
pub fn parse_spexus_file<P: AsRef<Path>>(path: P) -> Result<lang::syntax::Spexus, SpexusError> {
    let mut file = File::open(path)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;

    parse_spexus_string(&contents)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_spexus_string() -> Result<(), SpexusError> {
        let spexus = parse_spexus_string(
            r#"
            spec divide {
                prec: divisor != 0
                post: result == dividend / divisor
            }
        "#,
        )?;

        assert_eq!(spexus.entries.len(), 1);
        Ok(())
    }
}
