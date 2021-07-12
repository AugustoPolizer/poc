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

enum Declaration {
    Expr(Expression),
    Stmt(Statement),
}

enum Expression {
    Binary(BinaryExpr),
    Unary(UnaryExpr),
    Literal(LiteralExpr),
    Grouping(GroupingExpr), 
    FuncCall(FuncCallExpr),
}

struct FuncCallExpr {
    callee: Box<Expression>,
    args: Vec<Expression>,
}

impl FuncCallExpr {
    pub fn new(callee: Expression, args: Vec<Expression>) -> FuncCallExpr {
        FuncCallExpr { 
            callee: Box::new(callee), 
            args
        }
    }
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
    SkipUntilRParentheses,
    SkipCodeBlock,
    SkipCodeBlockIf,
    SkipSync,
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

    pub fn parse(&mut self) -> Result<Vec<Declaration>, Vec<ParsingError>> {
        let mut ast = Vec::new();
        let mut errors = Vec::new();

        // Init global scope
        self.scopes.create_new_scope();
        while !self.lexer.is_empty() {
            match self.declaration() {
                Ok(decl) => ast.push(decl),
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
                                internal_error_msg_handle(
                                    InternalErrorTypes::INVALIDSTATE,
                                    format!("{}", self.state).as_str(),
                                ),
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

    fn sync(&mut self) -> Result<(), ParsingError> {
        match self.state {
            State::Error(RecoverStrategy::SkipUntilDelimiter) => {
                self.lexer
                    .consume_until_find(lexer::TokenType::SEMICOLON, ";");
                Ok(())
            }
            State::Error(RecoverStrategy::SkipCodeBlock) => {
                self.lexer.consume_until_find(lexer::TokenType::RBRACE, "}");
                Ok(())
            }
            State::Error(RecoverStrategy::SkipCodeBlockIf) => {
                self.lexer.consume_until_find(lexer::TokenType::RBRACE, "}");
                let next_token = self.lexer.get_first_token();
                if next_token.token_type == lexer::TokenType::KEYWORD && next_token.text == "else" {
                    self.lexer.consume_until_find(lexer::TokenType::RBRACE, "}");
                }
                Ok(())
            }
            State::Error(RecoverStrategy::SkipUntilRParentheses) => {
                self.lexer
                    .consume_until_find(lexer::TokenType::RPARENTHESE, ")");
                Ok(())
            }
            State::Error(RecoverStrategy::SkipSync) => Ok(()),
            _ => {
                self.state = State::FatalError;
                Err(ParsingError::Internal(InternalError::new(
                    internal_error_msg_handle(
                        InternalErrorTypes::INVALIDSTATE,
                        format!("{}", self.state).as_str(),
                    ),
                )))
            }
        }
    }

    fn declaration(&mut self) -> Result<Declaration, ParsingError> {
        let token = self.lexer.get_first_token();
        if token.token_type == lexer::TokenType::KEYWORD {
            Ok(Declaration::Stmt(self.statement()?))
        } else {
            Ok(Declaration::Expr(self.expression()?))
        }
    }

    fn statement(&mut self) -> Result<Statement, ParsingError> {
        if let Some(match_result) = self.lexer.try_to_match_token(
            lexer::TokenType::KEYWORD,
            vec!["function", "let", "const", "if", "else", "return"],
        ) {
            match match_result.text.as_str() {
                "if" => self.if_stmt(),
                "return" => self.return_stmt(),
                "let" => self.var_decl(false),
                "const" => self.var_decl(true),
                "function" => self.func_decl(),
                "else" => {
                    return Err(ParsingError::UnexpectedToken(UnexpectedTokenError::new(
                        unexpected_token_error_msg(
                            UnexpectedTokenErrorTypes::ELSE,
                            &match_result.text,
                        ),
                        match_result.line,
                        match_result.column,
                    )));
                }
                _ => {
                    self.state = State::FatalError;
                    return Err(ParsingError::Internal(InternalError::new(
                        internal_error_msg_handle(
                            InternalErrorTypes::LEXERINVALIDTOKENVALUE,
                            format!("Found token {} as TokenType KEYWORD", match_result.text).as_str(),
                        ),
                    )));
                }
            }
        } else {
            return Err(ParsingError::Internal(InternalError::new(
                internal_error_msg_handle(
                    InternalErrorTypes::UNEXPECTEDERROR,
                    "Error in lexer internal state",
                ),
            )));
        }
    }

    fn func_decl(&mut self) -> Result<Statement, ParsingError> {
        let token = self.match_or_error(
            lexer::TokenType::NAME,
            "",
            UnexpectedTokenErrorTypes::FUNCNAME,
            RecoverStrategy::SkipCodeBlock,
        )?;

        self.match_or_error(
            lexer::TokenType::LPARENTHESE,
            "(",
            UnexpectedTokenErrorTypes::LPARENTHESE,
            RecoverStrategy::SkipCodeBlock,
        )?;

        let params = self.func_params()?;

        self.match_or_error(
            lexer::TokenType::RPARENTHESE,
            ")",
            UnexpectedTokenErrorTypes::RPARENTHESE,
            RecoverStrategy::SkipCodeBlock,
        )?;
        self.match_or_error(
            lexer::TokenType::LBRACE,
            "{",
            UnexpectedTokenErrorTypes::RPARENTHESE,
            RecoverStrategy::SkipCodeBlock,
        )?;

        let mut body = Vec::new();
        while let None = self
            .lexer
            .try_to_match_token(lexer::TokenType::RBRACE, vec!["}"])
        {
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

    // FIXME: Parse the function retorn type
    fn func_params(&mut self) -> Result<Vec<scope_manager::Param>, ParsingError> {
        let mut params = Vec::new();
        while let None = self
            .lexer
            .try_to_match_token(lexer::TokenType::RPARENTHESE, vec![")"])
        {
            if self.lexer.is_empty() {
                self.state = State::Error(RecoverStrategy::SkipSync);
                return Err(ParsingError::MissingToken(MissingTokenError::new(
                    missing_token_error_msg_handle(MissingTokenErrorTypes::RPARENTHESE),
                )));
            }
            let mut is_const = false;
            if let Some(_) = self
                .lexer
                .try_to_match_token(lexer::TokenType::KEYWORD, vec!["const"])
            {
                is_const = true;
            }

            let param_token = self.match_or_error(
                lexer::TokenType::NAME,
                "",
                UnexpectedTokenErrorTypes::PARAMNAME,
                RecoverStrategy::SkipCodeBlock,
            )?;
            self.match_or_error(
                lexer::TokenType::COLON,
                ":",
                UnexpectedTokenErrorTypes::COLON,
                RecoverStrategy::SkipCodeBlock,
            )?;
            let type_token = self.match_or_error(
                lexer::TokenType::TYPE,
                "",
                UnexpectedTokenErrorTypes::TYPE,
                RecoverStrategy::SkipCodeBlock,
            )?;

            self.lexer
                .try_to_match_token(lexer::TokenType::COMMA, vec![","]);
            params.push(scope_manager::Param::new(
                self.string_to_type(&type_token.text)?,
                is_const,
            ));
        }
        Ok(params)
    }


    fn if_stmt(&mut self) -> Result<Statement, ParsingError> {
        self.match_or_error(
            lexer::TokenType::LPARENTHESE,
            "(",
            UnexpectedTokenErrorTypes::LPARENTHESE,
            RecoverStrategy::SkipCodeBlock,
        )?;

        let expr = self.expression()?;

        self.match_or_error(
            lexer::TokenType::RPARENTHESE,
            ")",
            UnexpectedTokenErrorTypes::RPARENTHESE,
            RecoverStrategy::SkipCodeBlock,
        )?;
        self.match_or_error(
            lexer::TokenType::LBRACE,
            "{",
            UnexpectedTokenErrorTypes::LBRACE,
            RecoverStrategy::SkipCodeBlock,
        )?;

        let mut if_stmts = Vec::new();
        while let None = self
            .lexer
            .try_to_match_token(lexer::TokenType::RBRACE, vec!["}"])
        {
            if self.lexer.is_empty() {
                self.state = State::Error(RecoverStrategy::SkipSync);
                return Err(ParsingError::MissingToken(MissingTokenError::new(
                    missing_token_error_msg_handle(MissingTokenErrorTypes::RBRACE),
                )));
            }
            if_stmts.push(self.statement()?);
        }

        // TODO: Improves else parsing (e.g else if .....)
        // Check for else statement
        let mut else_stmts = Vec::new();
        if let Some(_) = self
            .lexer
            .try_to_match_token(lexer::TokenType::KEYWORD, vec!["else"])
        {
            self.match_or_error(
                lexer::TokenType::LBRACE,
                "{",
                UnexpectedTokenErrorTypes::LBRACE,
                RecoverStrategy::SkipCodeBlock,
            )?;
            while let None = self
                .lexer
                .try_to_match_token(lexer::TokenType::RBRACE, vec!["}"])
            {
                if self.lexer.is_empty() {
                    self.state = State::Error(RecoverStrategy::SkipSync);
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
            RecoverStrategy::SkipUntilDelimiter,
        )?;
        Ok(Statement::Return(ReturnStmt::new(expr)))
    }

    fn var_decl(&mut self, is_const: bool) -> Result<Statement, ParsingError> {
        let token = self.match_or_error(
            lexer::TokenType::NAME,
            "",
            UnexpectedTokenErrorTypes::VARNAME,
            RecoverStrategy::SkipUntilDelimiter,
        )?;

        if self.scopes.name_exist_in_current_scope(&token.text) {
            self.state = State::Error(RecoverStrategy::SkipUntilDelimiter);
            return Err(ParsingError::ScopeResolution(ScopeResolutionError::new(
                scope_error_msg_handle(ScopeResolutionErrorTypes::ALREADYDECLARED, &token.text),
                token.line,
                token.column,
            )));
        }

        self.match_or_error(
            lexer::TokenType::COLON,
            ":",
            UnexpectedTokenErrorTypes::COLON,
            RecoverStrategy::SkipUntilDelimiter,
        )?;

        let token_type = self.match_or_error(
            lexer::TokenType::TYPE,
            "",
            UnexpectedTokenErrorTypes::TYPE,
            RecoverStrategy::SkipUntilDelimiter,
        )?;

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
            RecoverStrategy::SkipUntilDelimiter,
        )?;
        Ok(Statement::VarDecl(VarDeclStmt::new(token.text, expr)))
    }

    //FIXME treat assignments as l-values
    fn attr_stmt(&mut self, var_name: String) -> Result<Statement, ParsingError> {
        let expr = self.expression()?;
        self.match_or_error(
            lexer::TokenType::SEMICOLON,
            ";",
            UnexpectedTokenErrorTypes::SEMICOLON,
            RecoverStrategy::SkipUntilDelimiter,
        )?;
        Ok(Statement::Attr(AttrStmt::new(var_name, expr)))
    }

    // Expression parsing functions
    fn expression(&mut self) -> Result<Expression, ParsingError> {
        self.equality()
    }

    fn equality(&mut self) -> Result<Expression, ParsingError> {
        let mut expr = self.comparison()?;

        while let Some(match_result) = self
            .lexer
            .try_to_match_token(lexer::TokenType::OP, vec!["==", "!="])
        {
            let right = self.comparison()?;
            expr = Expression::Binary(BinaryExpr::new(right, expr, match_result.text));
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expression, ParsingError> {
        let mut expr = self.term()?;

        while let Some(match_result) = self
            .lexer
            .try_to_match_token(lexer::TokenType::OP, vec![">", ">=", "<", "<="])
        {
            let right = self.term()?;
            expr = Expression::Binary(BinaryExpr::new(right, expr, match_result.text));
        }

        Ok(expr)
    }

    fn term(&mut self) -> Result<Expression, ParsingError> {
        let mut expr = self.factor()?;

        while let Some(match_result) = self
            .lexer
            .try_to_match_token(lexer::TokenType::OP, vec!["+", "-"])
        {
            let right = self.factor()?;
            expr = Expression::Binary(BinaryExpr::new(right, expr, match_result.text));
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expression, ParsingError> {
        let mut expr = self.unary()?;

        while let Some(match_result) = self
            .lexer
            .try_to_match_token(lexer::TokenType::OP, vec!["*", "/"])
        {
            let right = self.unary()?;
            expr = Expression::Binary(BinaryExpr::new(right, expr, match_result.text));
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expression, ParsingError> {
        if let Some(match_result) = self
            .lexer
            .try_to_match_token(lexer::TokenType::OP, vec!["!", "-"])
        {
            let right = self.unary()?;
            return Ok(Expression::Unary(UnaryExpr::new(right, match_result.text)));
        }

        self.func_call()
    }

    fn func_call(&mut self) -> Result<Expression, ParsingError> {
        let mut expr = self.primary()?;  

        while let None = self
            .lexer
            .try_to_match_token(lexer::TokenType::LPARENTHESE, vec!["("])
        {
            expr = self.func_arguments(expr)?;
        }

        Ok(expr)
    }

    fn func_arguments(&mut self, callee: Expression) -> Result<Expression, ParsingError> {
        let mut arguments = Vec::new();
        if let None = self
            .lexer
            .try_to_match_token(lexer::TokenType::RPARENTHESE, vec![")"]) {
                loop {
                    arguments.push(self.expression()?);
                    if let None = self.lexer.try_to_match_token(lexer::TokenType::COMMA, vec![","]) { break; }
                }
        }

        self.match_or_error(
            lexer::TokenType::RPARENTHESE, 
            ")", 
            UnexpectedTokenErrorTypes::RPARENTHESE, 
            RecoverStrategy::SkipUntilDelimiter)?;

        Ok(Expression::FuncCall(FuncCallExpr::new(callee, arguments)))

    }

    fn primary(&mut self) -> Result<Expression, ParsingError> {
        if let Some(match_result) = self
            .lexer
            .try_to_match_token(lexer::TokenType::INTEGER, vec![])
        {
            return Ok(Expression::Literal(LiteralExpr::new(match_result.text)));
        }
        if let Some(match_result) = self
            .lexer
            .try_to_match_token(lexer::TokenType::FLOAT, vec![])
        {
            return Ok(Expression::Literal(LiteralExpr::new(match_result.text)));
        }
        if let Some(match_result) = self
            .lexer
            .try_to_match_token(lexer::TokenType::STRING, vec![])
        {
            return Ok(Expression::Literal(LiteralExpr::new(match_result.text)));
        }

        if let Some(match_result) = self
            .lexer
            .try_to_match_token(lexer::TokenType::NAME, vec![])
        {
            return Ok(Expression::Literal(LiteralExpr::new(match_result.text)));
        }

        if let Some(match_result) = self
            .lexer
            .try_to_match_token(lexer::TokenType::LPARENTHESE, vec!["("])
        {
            let expr = self.expression()?;

            self.match_or_error(
                lexer::TokenType::RPARENTHESE,
                ")",
                UnexpectedTokenErrorTypes::RPARENTHESE,
                RecoverStrategy::SkipUntilDelimiter,
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
        recovery_strategy: RecoverStrategy,
    ) -> Result<lexer::Token, ParsingError> {
        match self.lexer.match_token(token_type, token_text) {
            Err(err) => {
                self.state = State::Error(recovery_strategy);
                Err(ParsingError::UnexpectedToken(UnexpectedTokenError::new(
                    unexpected_token_error_msg(error_type, &err.text),
                    err.line,
                    err.column,
                )))
            }
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
