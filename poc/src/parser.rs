mod errors;
mod lexer;
mod scope_manager;

use errors::{
    internal_error_msg_handle, missing_token_error_msg_handle, scope_error_msg_handle,
    unexpected_token_error_msg, InternalError, InternalErrorTypes, MissingTokenError,
    MissingTokenErrorTypes, ParsingError, ScopeResolutionError, ScopeResolutionErrorTypes,
    UnexpectedTokenError, UnexpectedTokenErrorTypes,
};
use std::fmt;

enum Expression {
    Binary(BinaryExpr),
    Unary(UnaryExpr),
    Literal(LiteralExpr),
    Grouping(GroupingExpr),
}

struct BinaryExpr {
    right: Box<Expression>,
    left: Box<Expression>,
    operator: String,
}

impl BinaryExpr {
    pub fn new(right: Expression, left: Expression, operator: String) -> BinaryExpr {
        BinaryExpr {
            right: Box::new(right),
            left: Box::new(left),
            operator,
        }
    }
}

struct UnaryExpr {
    right: Box<Expression>,
    operator: String,
}

impl UnaryExpr {
    pub fn new(right: Expression, operator: String) -> UnaryExpr {
        UnaryExpr {
            right: Box::new(right),
            operator,
        }
    }
}

struct LiteralExpr {
    value: String,
}

impl LiteralExpr {
    pub fn new(value: String) -> LiteralExpr {
        LiteralExpr { value }
    }
}

struct GroupingExpr {
    expr: Box<Expression>,
}

impl GroupingExpr {
    pub fn new(expr: Expression) -> GroupingExpr {
        GroupingExpr {
            expr: Box::new(expr),
        }
    }
}

enum Statement {
    If(IfStmt),
    FuncDecl(FuncDeclStmt),
    FuncCall(FuncCallStmt),
    Return(ReturnStmt),
    VarDecl(VarDeclStmt),
    Attr(AttrStmt),
}

struct IfStmt {
    if_stmts: Vec<Statement>,
    else_stmts: Vec<Statement>,
    expr: Expression,
}

impl IfStmt {
    pub fn new(if_stmts: Vec<Statement>, else_stmts: Vec<Statement>, expr: Expression) -> IfStmt {
        IfStmt {
            if_stmts,
            else_stmts,
            expr,
        }
    }
}

struct FuncDeclStmt {
    name: String,
    params: Vec<scope_manager::Param>,
    body: Vec<Statement>,
}

impl FuncDeclStmt {
    pub fn new(
        name: String,
        params: Vec<scope_manager::Param>,
        body: Vec<Statement>,
    ) -> FuncDeclStmt {
        FuncDeclStmt { name, params, body }
    }
}

struct FuncCallStmt {
    name: String,
    args: Vec<String>,
}

impl FuncCallStmt {
    pub fn new(name: String, args: Vec<String>) -> FuncCallStmt {
        FuncCallStmt { name, args }
    }
}

struct ReturnStmt {
    expr: Expression,
}

impl ReturnStmt {
    pub fn new(expr: Expression) -> ReturnStmt {
        ReturnStmt { expr }
    }
}

struct VarDeclStmt {
    name: String,
    expr: Option<Expression>,
}

impl VarDeclStmt {
    pub fn new(name: String, expr: Option<Expression>) -> VarDeclStmt {
        VarDeclStmt { name, expr }
    }
}

struct AttrStmt {
    name: String,
    expr: Expression,
}

impl AttrStmt {
    pub fn new(name: String, expr: Expression) -> AttrStmt {
        AttrStmt { name, expr }
    }
}

enum State {
    Parse,
    Error(RecoverStrategy),
    FatalError,
    End,
}

impl fmt::Display for State {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Parse => write!(f, "Parse"),
            Error => write!(f, "Error"),
            End => write!(f, "End"),
        }
    }
}

enum RecoverStrategy {
    SkipUntilDelimiter,
    SkipCodeBlock,
    SkipSync
}

struct Parser<'a> {
    lexer: lexer::Lexer<'a>,
    scopes: scope_manager::ScopeManager,
    state: State,
}

impl<'a> Parser<'a> {
    pub fn new(code: &str) -> Parser {
        Parser {
            lexer: lexer::Lexer::new(code),
            scopes: scope_manager::ScopeManager::new(),
            state: State::Parse,
        }
    }

    pub fn parse(&mut self) -> Result<Vec<Statement>, Vec<ParsingError>> {
        let mut ast = Vec::new();
        let mut errors = Vec::new();

        // Init global scope
        self.scopes.create_new_scope();
        while !self.lexer.is_empty() {
            match self.statement() {
                Ok(stmt) => ast.push(stmt),
                Err(error) => {
                    errors.push(error);
                    match self.state {
                        State::Error(_) => {
                            if let Err(sync_error) = self.sync() {
                                errors.push(sync_error);
                                return Err(errors);
                            } 
                            self.state = State::Parse;
                        }
                        State::FatalError => return Err(errors),
                        _ => {
                            self.state = State::FatalError;
                            errors.push(ParsingError::Internal(InternalError::new(
                                            internal_error_msg_handle(InternalErrorTypes::INVALIDSTATE, format!("{}", self.state))
                                        )));
                            return Err(errors);
                        }
                    }
                }
            };
        }
        self.scopes.remove_scope();

        if errors.is_empty() {
            return Ok(ast);
        }
        Err(errors)
    }

    // Cosume tokens until find a statement delimiter
    fn sync(&mut self) -> Result<(), ParsingError> {
        match self.state {
            State::Error(RecoverStrategy::SkipUntilDelimiter) => {
                self.lexer
                    .cosume_until_find(lexer::TokenType::SEMICOLON, ";");
                Ok(())
            },
            State::Error(RecoverStrategy::SkipCodeBlock) => {
                self.lexer.cosume_until_find(lexer::TokenType::RBRACE, "}");
                Ok(())
            },
            State::Error(RecoverStrategy::SkipSync) => Ok(()),
            _ => {
                self.state = State::FatalError;
                Err(ParsingError::Internal(InternalError::new(
                internal_error_msg_handle(
                    InternalErrorTypes::INVALIDSTATE,
                    format!("{}", self.state),
                ),
            )))
            }
        }
    }

    fn statement(&mut self) -> Result<Statement, ParsingError> {
        let mut match_result = self.lexer.try_to_match_token(
            lexer::TokenType::KEYWORD,
            vec!["function", "let", "const", "if", "return"],
        );

        if let Some(token) = match_result {
            return  match token.text.as_str() {
                "if" => self.if_stmt(),
                "return" => self.return_stmt(),
                "let" => self.var_decl(false),
                "const" => self.var_decl(true),
                "function" => self.func_decl(),
                _ => {
                    self.state = State::FatalError;
                    return Err(ParsingError::Internal(InternalError::new(
                        internal_error_msg_handle(
                            InternalErrorTypes::LEXERINVALIDTOKENVALUE,
                            format!("Found token {} as TokenType KEYWORD", token.text),
                        )
                    )));
                }
            };
        } else {
              
        }
    }

    fn func_decl(&mut self) -> Result<Statement, ParsingError> {
        match self.lexer.match_token(lexer::TokenType::NAME, "") {
            Ok(token) => {
                self.match_or_error(
                    lexer::TokenType::LPARENTHESE,
                    "(",
                    UnexpectedTokenErrorTypes::LPARENTHESE,
                )?;
                let params = self.parse_func_params()?;
                self.match_or_error(
                    lexer::TokenType::RPARENTHESE,
                    ")",
                    UnexpectedTokenErrorTypes::RPARENTHESE,
                )?;
                self.match_or_error(
                    lexer::TokenType::LBRACE,
                    "{",
                    UnexpectedTokenErrorTypes::RPARENTHESE,
                )?;

                let mut body = Vec::new();
                while let None = self.lexer.try_to_match_token(lexer::TokenType::RBRACE, vec!["}"]) {
                    if self.lexer.is_empty() {
                        self.state = State::Error(RecoverStrategy::SkipSync);
                        return Err(ParsingError::MissingToken(MissingTokenError::new(
                            missing_token_error_msg_handle(MissingTokenErrorTypes::RBRACE),
                        )));
                    }
                    body.push(self.statement()?);
                }

                Ok(Statement::FuncDecl(FuncDeclStmt::new(
                    token.text, params, body,
                )))
            }
            Err(token_error) => Err(ParsingError::UnexpectedToken(UnexpectedTokenError::new(
                unexpected_token_error_msg(UnexpectedTokenErrorTypes::FUNCNAME, &token_error.text),
                token_error.line,
                token_error.column,
            ))),
        }
    }

    // FIXME: Parse the function retorn type
    fn parse_func_params(&mut self) -> Result<Vec<scope_manager::Param>, ParsingError> {
        let mut params = Vec::new();
        while let None = self.lexer.try_to_match_token(lexer::TokenType::RPARENTHESE, vec![")"]) {
            if self.lexer.is_empty() {
                return Err(ParsingError::MissingToken(MissingTokenError::new(
                    missing_token_error_msg_handle(MissingTokenErrorTypes::RPARENTHESE),
                )));
            }
            let mut is_const = false;
            if let Some(_) = self.lexer.try_to_match_token(lexer::TokenType::KEYWORD, vec!["const"]) {
                is_const = true;
            }
                
            let param_token = self.match_or_error(
                lexer::TokenType::NAME,
                "",
                UnexpectedTokenErrorTypes::PARAMNAME,
            )?;
            self.match_or_error(
                lexer::TokenType::COLON,
                ":",
                UnexpectedTokenErrorTypes::COLON,
            )?;
            let type_token = self.match_or_error(lexer::TokenType::TYPE, "", UnexpectedTokenErrorTypes::TYPE)?;
            
            self.lexer.try_to_match_token(lexer::TokenType::COMMA, vec![","]);
            params.push(scope_manager::Param::new(
                self.string_to_type(&type_token.text)?,
                is_const,
            ));
        }
        Ok(params)
    }

    fn func_call(&mut self, func_name: String) -> Result<Statement, ParsingError> {
        let mut args = Vec::new();
        while let None = self.lexer.try_to_match_token(lexer::TokenType::RPARENTHESE, vec![")"]){
            if self.lexer.is_empty() {
                self.state = State::Error(RecoverStrategy::SkipSync);
                return Err(ParsingError::MissingToken(MissingTokenError::new(
                    missing_token_error_msg_handle(MissingTokenErrorTypes::RPARENTHESE),
                )));
            }
            let arg_token = self.match_or_error(
                lexer::TokenType::NAME,
                "",
                UnexpectedTokenErrorTypes::ARGNAME,
            )?;
            self.lexer
                .try_to_match_token(lexer::TokenType::COMMA, vec![]);
            args.push(arg_token.text);
        }

        Ok(Statement::FuncCall(FuncCallStmt::new(func_name, args)))
    }

    fn if_stmt(&mut self) -> Result<Statement, ParsingError> {
        self.match_or_error(
            lexer::TokenType::LPARENTHESE,
            "(",
            UnexpectedTokenErrorTypes::LPARENTHESE,
        )?;

        let expr = self.expression()?;

        self.match_or_error(
            lexer::TokenType::RPARENTHESE,
            ")",
            UnexpectedTokenErrorTypes::RPARENTHESE,
        )?;
        self.match_or_error(
            lexer::TokenType::LBRACE,
            "{",
            UnexpectedTokenErrorTypes::LBRACE,
        )?;

        let mut if_stmts = Vec::new();
        while let None = self.lexer.try_to_match_token(lexer::TokenType::RBRACE, vec!["}"]) {
            if self.lexer.is_empty() {
                return Err(ParsingError::MissingToken(MissingTokenError::new(
                    missing_token_error_msg_handle(MissingTokenErrorTypes::RBRACE),
                )));
            }
            if_stmts.push(self.statement()?);
        }

        // TODO: Improves else parsing (e.g else if .....)
        // Check for else statement
        let mut else_stmts = Vec::new();
        if let Some(_) = self.lexer.try_to_match_token(lexer::TokenType::KEYWORD, vec!["else"]) {
            self.match_or_error(
                lexer::TokenType::LBRACE,
                "{",
                UnexpectedTokenErrorTypes::LBRACE,
            )?;
            while let None = self.lexer.try_to_match_token(lexer::TokenType::RBRACE, vec!["}"]) {
                if let Some(_) = self.lexer.try_to_match_token(lexer::TokenType::RBRACE, vec!["}"]) { break; }
                if self.lexer.is_empty() {
                    return Err(ParsingError::MissingToken(MissingTokenError::new(
                        missing_token_error_msg_handle(MissingTokenErrorTypes::RBRACE),
                    )));
                }
                else_stmts.push(self.statement()?);
            }
        }
        Ok(Statement::If(IfStmt::new(if_stmts, else_stmts, expr)))
    }

    fn return_stmt(&mut self) -> Result<Statement, ParsingError> {
        let expr = self.expression()?;
        self.match_or_error(
            lexer::TokenType::SEMICOLON,
            ";",
            UnexpectedTokenErrorTypes::SEMICOLON,
        )?;
        Ok(Statement::Return(ReturnStmt::new(expr)))
    }

    fn var_decl(&mut self, is_const: bool) -> Result<Statement, ParsingError> {
        match self.lexer.match_token(lexer::TokenType::NAME, "") {
            Ok(token) => {
                if self.scopes.name_exist_in_current_scope(&token.text) {
                    return Err(ParsingError::ScopeResolution(ScopeResolutionError::new(
                        scope_error_msg_handle(
                            ScopeResolutionErrorTypes::ALREADYDECLARED,
                            &token.text,
                        ),
                        token.line,
                        token.column,
                    )));
                }

                self.match_or_error(
                    lexer::TokenType::COLON,
                    ":",
                    UnexpectedTokenErrorTypes::COLON,
                )?;

                match self.lexer.match_token(lexer::TokenType::TYPE, "") {
                    Ok(token_type) => {
                        self.scopes.create_new_symbol(
                            self.string_to_type(&token_type.text)?,
                            &token.text,
                            is_const,
                        )?;
                        // var optional initialization
                        let mut expr = None;
                        if let Some(_) = self
                            .lexer
                            .try_to_match_token(lexer::TokenType::ATTR, vec!["="])
                        {
                            expr = Some(self.expression()?);
                        }

                        self.match_or_error(
                            lexer::TokenType::SEMICOLON,
                            ";",
                            UnexpectedTokenErrorTypes::SEMICOLON,
                        )?;
                        Ok(Statement::VarDecl(VarDeclStmt::new(token.text, expr)))
                    }
                    Err(err) => {
                        return Err(ParsingError::UnexpectedToken(UnexpectedTokenError::new(
                            unexpected_token_error_msg(UnexpectedTokenErrorTypes::TYPE, &err.text),
                            err.line,
                            err.column,
                        )));
                    }
                }
            }
            Err(err) => {
                return Err(ParsingError::UnexpectedToken(UnexpectedTokenError::new(
                    unexpected_token_error_msg(UnexpectedTokenErrorTypes::VARNAME, &err.text),
                    err.line,
                    err.column,
                )));
            }
        }
    }

    //FIXME treat assignments as l-values
    fn attr_stmt(&mut self, var_name: String) -> Result<Statement, ParsingError> {
        let expr = self.expression()?;
        self.match_or_error(
            lexer::TokenType::SEMICOLON,
            ";",
            UnexpectedTokenErrorTypes::SEMICOLON,
        )?;
        Ok(Statement::Attr(AttrStmt::new(var_name, expr)))
    }

    // Expression parsing functions
    fn expression(&mut self) -> Result<Expression, ParsingError> {
        self.equality()
    }

    fn equality(&mut self) -> Result<Expression, ParsingError> {
       let mut expr = self.comparison()?;

        while let Some(match_result) = self.lexer.try_to_match_token(lexer::TokenType::OP, vec!["==", "!="]) {
            let right = self.comparison()?;
            expr = Expression::Binary(BinaryExpr::new(right, expr, match_result.text));
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expression, ParsingError> {
        let mut expr = self.term()?;

        while let Some(match_result) = self.lexer.try_to_match_token(lexer::TokenType::OP, vec![">", ">=", "<", "<="]) {
            let right = self.term()?;
            expr = Expression::Binary(BinaryExpr::new(right, expr, match_result.text));
        }

        Ok(expr)
    }

    fn term(&mut self) -> Result<Expression, ParsingError> {
        let mut expr = self.factor()?;

        while let Some(match_result) = self.lexer.try_to_match_token(lexer::TokenType::OP, vec!["+", "-"]) {
            let right = self.factor()?;
            expr = Expression::Binary(BinaryExpr::new(right, expr, match_result.text));
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expression, ParsingError> {
        let mut expr = self.unary()?;

        while let Some(match_result) = self.lexer.try_to_match_token(lexer::TokenType::OP, vec!["*", "/"]) {
            let right = self.unary()?;
            expr = Expression::Binary(BinaryExpr::new(right, expr, match_result.text));
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expression, ParsingError> {
        if let Some(match_result) = self.lexer.try_to_match_token(lexer::TokenType::OP, vec!["!", "-"]) {
            let right = self.unary()?;
            return Ok(Expression::Unary(UnaryExpr::new(
                right,
                match_result.text,
            )));
        }

        self.primary()
    }

    //FIXME Add function call, variable names as valid primary values
    fn primary(&mut self) -> Result<Expression, ParsingError> {
        if let Some(match_result) = self.lexer.try_to_match_token(lexer::TokenType::INTEGER, vec![]) {
            return Ok(Expression::Literal(LiteralExpr::new(match_result.text)));
        }
        if let Some(match_result) = self.lexer.try_to_match_token(lexer::TokenType::FLOAT, vec![]) {
            return Ok(Expression::Literal(LiteralExpr::new(match_result.text)));
        }
        if let Some(match_result) = self.lexer.try_to_match_token(lexer::TokenType::STRING, vec![]) {
            return Ok(Expression::Literal(LiteralExpr::new(match_result.text)));
        }

        if let Some(match_result) = self.lexer.try_to_match_token(lexer::TokenType::LPARENTHESE, vec![]) {
            let expr = self.expression()?;

            self.match_or_error(
                lexer::TokenType::RPARENTHESE,
                ")",
                UnexpectedTokenErrorTypes::RPARENTHESE,
            )?;
            return Ok(Expression::Grouping(GroupingExpr::new(expr)));
        } else {
            let error_token = self.lexer.get_first_token();

            self.state = State::Error(RecoverStrategy::SkipUntilDelimiter);
            return Err(ParsingError::UnexpectedToken(UnexpectedTokenError::new(
                unexpected_token_error_msg(
                    UnexpectedTokenErrorTypes::UNEXPECTEDTOKEN,
                    &error_token.text,
                ),
                error_token.line,
                error_token.column,
            )));
        }
    }

    fn match_or_error(
        &mut self,
        token_type: lexer::TokenType,
        token_text: &str,
        error_type: UnexpectedTokenErrorTypes,
    ) -> Result<lexer::Token, ParsingError> { 
        match self.lexer.match_token(token_type, token_text) {
            Err(err) => {
                self.state = State::Error(RecoverStrategy::SkipUntilDelimiter);
                Err(ParsingError::UnexpectedToken(UnexpectedTokenError::new(
                unexpected_token_error_msg(error_type, &err.text),
                err.line,
                err.column,
            )))},
            Ok(token) => Ok(token),
        }
    }

    fn string_to_type(&self, type_string: &str) -> Result<scope_manager::Type, ParsingError> {
        match type_string {
            "int" => Ok(scope_manager::Type::INTEGER),
            "float" => Ok(scope_manager::Type::FLOAT),
            "string" => Ok(scope_manager::Type::STRING),
            _ => Err(ParsingError::Internal(InternalError::new(
                internal_error_msg_handle(
                    InternalErrorTypes::UNEXPECTEDERROR,
                    format!("Found a unknown type at the parsing: {}", type_string).as_str(),
                ),
            ))),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
}
