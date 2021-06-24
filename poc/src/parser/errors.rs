use std::fmt;
pub enum UnexpectedTokenErrorTypes {
    FUNCNAME,
    VARNAME,
    PARAMNAME,
    ARGNAME,
    TYPE,
    COLON,
    SEMICOLON,
    LPARENTHESE,
    RPARENTHESE,
    LBRACE,
    COMMAORRPARENTHESE,
    UNEXPECTEDKEYWORD,
    UNEXPECTEDTOKEN,
}

pub enum MissingTokenErrorTypes {
    RBRACE,
    RPARENTHESE,
}

pub enum ScopeResolutionErrorTypes {
    ALREADYDECLARED,
}

pub enum InternalErrorTypes {
    UNABLETOINSERTSYMBOL,
    UNEXPECTEDERROR,
}

pub fn unexpected_token_error_msg(
    error_type: UnexpectedTokenErrorTypes,
    wrong_token: &str,
) -> String {
    match error_type {
        UnexpectedTokenErrorTypes::FUNCNAME => {
            format!("Expected a function name, found \"{}\"", wrong_token)
        }
        UnexpectedTokenErrorTypes::PARAMNAME => {
            format!("Expected a function param, found \"{}\"", wrong_token)
        }
        UnexpectedTokenErrorTypes::ARGNAME => {
            format!("Expected a function argument, found \"{}\"", wrong_token)
        }
        UnexpectedTokenErrorTypes::VARNAME => {
            format!("Expected a variable name, found \"{}\"", wrong_token)
        }
        UnexpectedTokenErrorTypes::TYPE => format!("Expected a type, found \"{}\"", wrong_token),
        UnexpectedTokenErrorTypes::COLON => format!("Expected a \":\", found \"{}\"", wrong_token),
        UnexpectedTokenErrorTypes::SEMICOLON => {
            format!("Expected a \";\", found \"{}\"", wrong_token)
        }
        UnexpectedTokenErrorTypes::LPARENTHESE => {
            format!("Expected a \"(\", found \"{}\"", wrong_token)
        }
        UnexpectedTokenErrorTypes::RPARENTHESE => {
            format!("Expected a \")\", found \"{}\"", wrong_token)
        }
        UnexpectedTokenErrorTypes::LBRACE => {
            format!("Expected a \"{{\", found \"{}\"", wrong_token)
        }
        UnexpectedTokenErrorTypes::COMMAORRPARENTHESE => {
            format!("Expected a \",\" or a \")\", found \"{}\"", wrong_token)
        }
        UnexpectedTokenErrorTypes::UNEXPECTEDKEYWORD => {
            format!("Unexpected keyword found: \"{}\"", wrong_token)
        }
        UnexpectedTokenErrorTypes::UNEXPECTEDTOKEN => {
            format!("Unexpected token found: \"{}\"", wrong_token)
        }
    }
}

pub fn scope_error_msg_handle(error_type: ScopeResolutionErrorTypes, error: &str) -> String {
    match error_type {
        ScopeResolutionErrorTypes::ALREADYDECLARED => {
            format!("Identifier \"{}\" has already been declared", error)
        }
    }
}

pub fn internal_error_msg_handle(error_type: InternalErrorTypes, error: &str) -> String {
    match error_type {
        InternalErrorTypes::UNABLETOINSERTSYMBOL => format!(
            "Internal error: Unable to insert symbol into scope: {}",
            error
        ),
        InternalErrorTypes::UNEXPECTEDERROR => format!(
            "Internal error: An unexpected error has occurred: {}",
            error
        ),
    }
}

pub fn missing_token_error_msg_handle(error_type: MissingTokenErrorTypes) -> String {
    match error_type {
        MissingTokenErrorTypes::RBRACE => {
            String::from("Sudden end of input. Missing a closing \"}\"")
        }
        MissingTokenErrorTypes::RPARENTHESE => {
            String::from("Sudden end of input. Missing a closing \")\"")
        }
    }
}

pub enum ParsingError {
    UnexpectedToken(UnexpectedTokenError),
    ScopeResolution(ScopeResolutionError),
    MissingToken(MissingTokenError),
    Internal(InternalError),
}

impl From<UnexpectedTokenError> for ParsingError {
    fn from(error: UnexpectedTokenError) -> Self {
        ParsingError::UnexpectedToken(error)
    }
}

impl From<ScopeResolutionError> for ParsingError {
    fn from(error: ScopeResolutionError) -> Self {
        ParsingError::ScopeResolution(error)
    }
}

impl From<MissingTokenError> for ParsingError {
    fn from(error: MissingTokenError) -> Self {
        ParsingError::MissingToken(error)
    }
}

impl From<InternalError> for ParsingError {
    fn from(error: InternalError) -> Self {
        ParsingError::Internal(error)
    }
}

#[derive(Debug, Clone)]
pub struct UnexpectedTokenError {
    error_msg: String,
    line: u32,
    column: u32,
}

impl fmt::Display for UnexpectedTokenError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "line:{}: {}", self.line, self.error_msg)
    }
}

impl UnexpectedTokenError {
    pub fn new(error_msg: String, line: u32, column: u32) -> UnexpectedTokenError {
        UnexpectedTokenError {
            error_msg,
            line,
            column,
        }
    }
}

#[derive(Debug, Clone)]
pub struct MissingTokenError {
    error_msg: String,
}

impl fmt::Display for MissingTokenError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "InternalError: {}", self.error_msg)
    }
}

impl MissingTokenError {
    pub fn new(error_msg: String) -> MissingTokenError {
        MissingTokenError { error_msg }
    }
}

#[derive(Debug, Clone)]
pub struct InternalError {
    error_msg: String,
}

impl fmt::Display for InternalError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "InternalError: {}", self.error_msg)
    }
}

impl InternalError {
    pub fn new(error_msg: String) -> InternalError {
        InternalError { error_msg }
    }
}

#[derive(Debug, Clone)]
pub struct ScopeResolutionError {
    error_msg: String,
    line: u32,
    column: u32,
}

impl fmt::Display for ScopeResolutionError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "line:{}: {}", self.line, self.error_msg)
    }
}

impl ScopeResolutionError {
    pub fn new(error_msg: String, line: u32, column: u32) -> ScopeResolutionError {
        ScopeResolutionError {
            error_msg,
            line,
            column,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // UnexpectedTokenError
    #[test]
    fn func_name_error_msg() {
        let error_msg = unexpected_token_error_msg(UnexpectedTokenErrorTypes::FUNCNAME, "*");
        assert_eq!(error_msg, "Expected a function name, found \"*\"")
    }

    #[test]
    fn param_name_error_msg() {
        let error_msg = unexpected_token_error_msg(UnexpectedTokenErrorTypes::PARAMNAME, "+");
        assert_eq!(error_msg, "Expected a function param, found \"+\"")
    }

    #[test]
    fn arg_name_error_msg() {
        let error_msg = unexpected_token_error_msg(UnexpectedTokenErrorTypes::ARGNAME, "+");
        assert_eq!(error_msg, "Expected a function argument, found \"+\"")
    }
    #[test]
    fn var_name_error_msg() {
        let error_msg = unexpected_token_error_msg(UnexpectedTokenErrorTypes::VARNAME, ":");
        assert_eq!(error_msg, "Expected a variable name, found \":\"")
    }

    #[test]
    fn type_error_msg() {
        let error_msg = unexpected_token_error_msg(UnexpectedTokenErrorTypes::TYPE, "let");
        assert_eq!(error_msg, "Expected a type, found \"let\"")
    }

    #[test]
    fn colon_error_msg() {
        let error_msg = unexpected_token_error_msg(UnexpectedTokenErrorTypes::COLON, "function");
        assert_eq!(error_msg, "Expected a \":\", found \"function\"")
    }

    #[test]
    fn semicollon_error_msg() {
        let error_msg = unexpected_token_error_msg(UnexpectedTokenErrorTypes::SEMICOLON, ":");
        assert_eq!(error_msg, "Expected a \";\", found \":\"")
    }

    #[test]
    fn lparenthese_error_msg() {
        let error_msg =
            unexpected_token_error_msg(UnexpectedTokenErrorTypes::LPARENTHESE, "var_name");
        assert_eq!(error_msg, "Expected a \"(\", found \"var_name\"")
    }

    #[test]
    fn rparenthese_error_msg() {
        let error_msg =
            unexpected_token_error_msg(UnexpectedTokenErrorTypes::RPARENTHESE, "var_name");
        assert_eq!(error_msg, "Expected a \")\", found \"var_name\"")
    }

    #[test]
    fn lbrace_error_msg() {
        let error_msg = unexpected_token_error_msg(UnexpectedTokenErrorTypes::LBRACE, "var_name");
        assert_eq!(error_msg, "Expected a \"{\", found \"var_name\"")
    }

    #[test]
    fn comma_or_rparenthese_error_msg() {
        let error_msg =
            unexpected_token_error_msg(UnexpectedTokenErrorTypes::COMMAORRPARENTHESE, "var_name");
        assert_eq!(error_msg, "Expected a \",\" or a \")\", found \"var_name\"")
    }

    #[test]
    fn unexpected_token() {
        let error_msg =
            unexpected_token_error_msg(UnexpectedTokenErrorTypes::UNEXPECTEDTOKEN, "token");
        assert_eq!(error_msg, "Unexpected token found: \"token\"");
    }

    #[test]
    fn unexpected_keyword() {
        let error_msg =
            unexpected_token_error_msg(UnexpectedTokenErrorTypes::UNEXPECTEDKEYWORD, "keyword");
        assert_eq!(error_msg, "Unexpected keyword found: \"keyword\"");
    }

    // MissingToken
    #[test]
    fn missing_token_r_brace_error_msg() {
        let error_msg = missing_token_error_msg_handle(MissingTokenErrorTypes::RBRACE);
        assert_eq!(error_msg, "Sudden end of input. Missing a closing \"}\"")
    }

    // ScopeError
    #[test]
    fn already_been_declared() {
        let error_msg =
            scope_error_msg_handle(ScopeResolutionErrorTypes::ALREADYDECLARED, "var_name");
        assert_eq!(
            error_msg,
            "Identifier \"var_name\" has already been declared"
        );
    }

    // InternalError
    #[test]
    fn unable_to_insert_symbol() {
        let error_msg = internal_error_msg_handle(
            InternalErrorTypes::UNABLETOINSERTSYMBOL,
            "There is no scope to insert symbol",
        );
        assert_eq!(error_msg, "Internal error: Unable to insert symbol into scope: There is no scope to insert symbol");
    }

    #[test]
    fn unexpected_internal_error() {
        let error_msg =
            internal_error_msg_handle(InternalErrorTypes::UNEXPECTEDERROR, "error msg test");
        assert_eq!(
            error_msg,
            "Internal error: An unexpected error has occurred: error msg test"
        );
    }
}
