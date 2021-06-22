pub mod error_msgs{
    pub enum UnexpectedTokenError {
        FUNCNAME,
        VARNAME,
        PARAMNAME,
        TYPE,
        COLON,
        SEMICOLON,
        LPARENTHESE,
        RPARENTHESE,
        LBRACE,
        COMMAORRPARENTHESE,
        UNEXPECTEDKEYWORD,
        UNEXPECTEDTOKEN
    }

    pub enum MissingTokenError {
        RBRACE
    }

    pub enum ScopeError {
        ALREADYDECLARED,
    }

    pub enum InternalError {
        UNABLETOINSERTSYMBOL,
        UNABLETOCREATESYMBOL,
    }


    pub fn wrong_token_error_msg_handle(error_type: UnexpectedTokenError, wrong_token: &str) -> String {
        match error_type {
            UnexpectedTokenError::FUNCNAME => format!("Expected a function name, found \"{}\"", wrong_token),
            UnexpectedTokenError::PARAMNAME => format!("Expected a param name, found \"{}\"", wrong_token),
            UnexpectedTokenError::VARNAME => format!("Expected a variable name, found \"{}\"", wrong_token),
            UnexpectedTokenError::TYPE => format!("Expected a type, found \"{}\"", wrong_token),
            UnexpectedTokenError::COLON => format!("Expected a \":\", found \"{}\"", wrong_token),
            UnexpectedTokenError::SEMICOLON => format!("Expected a \";\", found \"{}\"", wrong_token),
            UnexpectedTokenError::LPARENTHESE => format!("Expected a \"(\", found \"{}\"", wrong_token),
            UnexpectedTokenError::RPARENTHESE => format!("Expected a \")\", found \"{}\"", wrong_token),
            UnexpectedTokenError::LBRACE => format!("Expected a \"{{\", found \"{}\"", wrong_token),
            UnexpectedTokenError::COMMAORRPARENTHESE => format!("Expected a \",\" or a \")\", found \"{}\"", wrong_token),
            UnexpectedTokenError::UNEXPECTEDKEYWORD => format!("Unexpected keyword found: \"{}\"", wrong_token),
            UnexpectedTokenError::UNEXPECTEDTOKEN => format!("Unexpected token found: \"{}\"", wrong_token)
        }
    }

    pub fn scope_error_msg_handle(error_type: ScopeError, error: &str) -> String {
        match error_type {
                ScopeError::ALREADYDECLARED => format!("Identifier \"{}\" has already been declared", error),
        }
    }

    pub fn internal_error_msg_handle(error_type: InternalError, error: &str) -> String {
        match error_type {
            InternalError::UNABLETOINSERTSYMBOL => format!("Internal error: Unable to insert symbol into scope: {}", error),
            InternalError::UNABLETOCREATESYMBOL => format!("Internal error: Unable to create symbol: {}", error),
        }
    }

    pub fn missing_token_error_msg_handle(error_type: MissingTokenError) -> String{
        match error_type {
            MissingTokenError::RBRACE => String::from("Sudden end of input. Missing a closing \"}\""),
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        // UnexpectedTokenError 
        #[test]
        fn func_name_error_msg() {
            let error_msg = wrong_token_error_msg_handle(UnexpectedTokenError::FUNCNAME, "*");
            assert_eq!(error_msg, "Expected a function name, found \"*\"")
        }

        #[test] 
        fn param_name_error_msg() {
            let error_msg = wrong_token_error_msg_handle(UnexpectedTokenError::PARAMNAME, "+");
            assert_eq!(error_msg, "Expected a param name, found \"+\"")
        }
        
        #[test] 
        fn var_name_error_msg() {
            let error_msg = wrong_token_error_msg_handle(UnexpectedTokenError::VARNAME, ":");
            assert_eq!(error_msg, "Expected a variable name, found \":\"")
        }
        
        #[test]
        fn type_error_msg() {
            let error_msg = wrong_token_error_msg_handle(UnexpectedTokenError::TYPE, "let");
            assert_eq!(error_msg, "Expected a type, found \"let\"")
        }

        #[test]
        fn colon_error_msg() {
            let error_msg = wrong_token_error_msg_handle(UnexpectedTokenError::COLON, "function");
            assert_eq!(error_msg, "Expected a \":\", found \"function\"")
        }

        #[test]
        fn semicollon_error_msg() {
            let error_msg = wrong_token_error_msg_handle(UnexpectedTokenError::SEMICOLON, ":");
            assert_eq!(error_msg, "Expected a \";\", found \":\"")
        }

        #[test]
        fn lparenthese_error_msg() {
            let error_msg = wrong_token_error_msg_handle(UnexpectedTokenError::LPARENTHESE, "var_name");
            assert_eq!(error_msg, "Expected a \"(\", found \"var_name\"")
        }
        
        #[test]
        fn rparenthese_error_msg() {
            let error_msg = wrong_token_error_msg_handle(UnexpectedTokenError::RPARENTHESE, "var_name");
            assert_eq!(error_msg, "Expected a \")\", found \"var_name\"")
        }

        #[test]
        fn lbrace_error_msg() {
            let error_msg = wrong_token_error_msg_handle(UnexpectedTokenError::LBRACE, "var_name");
            assert_eq!(error_msg, "Expected a \"{\", found \"var_name\"")
        }

        #[test]
        fn comma_or_rparenthese_error_msg() {
            let error_msg = wrong_token_error_msg_handle(UnexpectedTokenError::COMMAORRPARENTHESE, "var_name");
            assert_eq!(error_msg, "Expected a \",\" or a \")\", found \"var_name\"")
        }

        #[test]
        fn unexpected_token() {
            let error_msg = wrong_token_error_msg_handle(UnexpectedTokenError::UNEXPECTEDTOKEN, "token");
            assert_eq!(error_msg, "Unexpected token found: \"token\"");
        }

        #[test]
        fn unexpected_keyword() {
            let error_msg = wrong_token_error_msg_handle(UnexpectedTokenError::UNEXPECTEDKEYWORD, "keyword");
            assert_eq!(error_msg, "Unexpected keyword found: \"keyword\"");
        }

        // MissingToken 
        #[test]
        fn missing_token_r_brace_error_msg() {
            let error_msg = missing_token_error_msg_handle(MissingTokenError::RBRACE);
            assert_eq!(error_msg, "Sudden end of input. Missing a closing \"}\"")
        }

        // ScopeError
        #[test]
        fn already_been_declared() {
            let error_msg = scope_error_msg_handle(ScopeError::ALREADYDECLARED, "var_name");
            assert_eq!(error_msg, "Identifier \"var_name\" has already been declared");
        }

        // InternalError
        #[test]
        fn unable_to_insert_symbol() {
            let error_msg = internal_error_msg_handle(
                InternalError::UNABLETOINSERTSYMBOL, 
                "There is no scope to insert symbol"
                );
            assert_eq!(error_msg, "Internal error: Unable to insert symbol into scope: There is no scope to insert symbol");
        }
        
        #[test]
        fn unable_to_create_symbol() {
            let error_msg = internal_error_msg_handle(
                InternalError::UNABLETOCREATESYMBOL, 
                "Unknown type \"u32\""
                );
            assert_eq!(error_msg, "Internal error: Unable to create symbol: Unknown type \"u32\"");
        }
        
    }
}

use std::fmt;

pub enum ParsingError{
    UnexpectedToken(UnexpectedTokenError),
    ScopeResolution(ScopeResolutionError),
    MissingToken(MissingTokenError),
    Internal(InternalError)
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
    fn from (error: InternalError) -> Self {
        ParsingError::Internal(error)
    }
}

#[derive(Debug, Clone)]
pub struct UnexpectedTokenError {
    error_msg: String,
    line: u32,
    column: u32
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
            column
        }
    }
} 

#[derive(Debug, Clone)]
pub struct MissingTokenError {
    error_msg: String
}

impl fmt::Display for MissingTokenError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "InternalError: {}", self.error_msg)
    }
}

impl MissingTokenError {
    pub fn new(error_msg: String) -> MissingTokenError {
        MissingTokenError {
            error_msg
        }
    }
}

#[derive(Debug, Clone)]
pub struct InternalError {
    error_msg: String
}

impl fmt::Display for InternalError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "InternalError: {}", self.error_msg)
    }
}

impl InternalError {
    pub fn new(error_msg: String) -> InternalError {
        InternalError {
            error_msg
        }
    }
}

#[derive(Debug, Clone)]
pub struct ScopeResolutionError {
    error_msg: String,
    line: u32,
    column: u32
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
            column
        }
    }
} 
