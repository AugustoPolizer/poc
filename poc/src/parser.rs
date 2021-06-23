mod errors;
mod lexer;
mod scope_manager;

use errors::{
    internal_error_msg_handle, missing_token_error_msg_handle, scope_error_msg_handle,
    unexpected_token_error_msg, InternalError, InternalErrorTypes, MissingTokenError,
    MissingTokenErrorTypes, ParsingError, ScopeResolutionError, ScopeResolutionErrorTypes,
    UnexpectedTokenError, UnexpectedTokenErrorTypes,
};

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
    params: Vec<String>,
    body: Vec<Statement>,
}

impl FuncDeclStmt {
    pub fn new(name: String, params: Vec<String>, body: Vec<Statement>) -> FuncDeclStmt{
        FuncDeclStmt {
            name,
            params,
            body
        }
    }
}

struct FuncCallStmt {
    func_name: String,
    func_args: Vec<String>,
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

// Used only as return type of function find_name_in_current_scope
enum ScopeTypes {
    Symbol(scope_manager::Symbol),
    FuncDecl(scope_manager::FuncDecl),
}

struct Parser<'a> {
    lexer: lexer::Lexer<'a>,
    scopes: scope_manager::ScopeManager,
}

impl<'a> Parser<'a> {
    pub fn new(code: &str) -> Parser {
        Parser {
            lexer: lexer::Lexer::new(code),
            scopes: scope_manager::ScopeManager::new(),
        }
    }

    pub fn parse(&mut self) -> Result<Vec<Statement>, Vec<ParsingError>> {
        let mut ast = Vec::new();
        let mut errors = Vec::new();

        while !self.lexer.is_empty() {
            match self.statement() {
                Ok(stmt) => ast.push(stmt),
                Err(error) => {
                    errors.push(error);
                    self.sync();
                }
            }
        }

        if errors.is_empty() {
            return Ok(ast);
        }
        Err(errors)
    }

    // Cosume tokens until find a statement delimiter
    fn sync(&mut self) {
        // Panic mode
    }

    fn statement(&mut self) -> Result<Statement, ParsingError> {
        let mut match_result = self.lexer.try_to_match_token(
            lexer::TokenType::KEYWORD,
            vec!["function", "let", "const", "if", "return"],
        );

        return match match_result.1.text.as_str() {
            "if" => self.if_stmt(),
            "return" => self.return_stmt(),
            "let" => self.var_decl(false),
            "const" => self.var_decl(true),
            "function" =>  self.func_decl(),
            _ => {
                match_result = self
                    .lexer
                    .try_to_match_token(lexer::TokenType::NAME, vec![]);
                if match_result.0 {
                    let check_attr = self
                        .lexer
                        .try_to_match_token(lexer::TokenType::ATTR, vec!["="]);
                    if check_attr.0 {
                        return self.attr_stmt(match_result.1.text);
                    }
                    match_result = self
                        .lexer
                        .try_to_match_token(lexer::TokenType::LPARENTHESE, vec!["("]);
                    if match_result.0 {
                        // return self.func_call();
                    }
                }

                return Err(ParsingError::UnexpectedToken(UnexpectedTokenError::new(
                    unexpected_token_error_msg(
                        UnexpectedTokenErrorTypes::UNEXPECTEDTOKEN,
                        &match_result.1.text,
                    ),
                    match_result.1.line,
                    match_result.1.column,
                )));
            }
        };
    }

    fn func_decl(&mut self) -> Result<Statement, ParsingError> {
       match self.lexer.match_token(lexer::TokenType::NAME, "") {
           Ok(token) => {
               self.error_or_match(lexer::TokenType::LPARENTHESE, "(", UnexpectedTokenErrorTypes::LPARENTHESE)?;
               let params = self.parse_func_params()?;
               self.error_or_match(lexer::TokenType::RPARENTHESE, ")", UnexpectedTokenErrorTypes::RPARENTHESE)?;
               self.error_or_match(lexer::TokenType::LBRACE, "{", UnexpectedTokenErrorTypes::RPARENTHESE)?;
               
                let mut body = Vec::new();
                while !self
                    .lexer
                    .try_to_match_token(lexer::TokenType::RBRACE, vec!["}"])
                    .0
                {
                    if self.lexer.is_empty() {
                        return Err(ParsingError::MissingToken(MissingTokenError::new(
                            missing_token_error_msg_handle(MissingTokenErrorTypes::RBRACE),
                        )));
                    }
                    body.push(self.statement()?);
                }

               Ok(Statement::FuncDecl(FuncDeclStmt::new(token.text, params, body)))
           },
           Err(token_error) => {
               Err(ParsingError::UnexpectedToken(
                       UnexpectedTokenError::new(unexpected_token_error_msg(
                               UnexpectedTokenErrorTypes::FUNCNAME, &token_error.text),token_error.line, token_error.column)))
           }
       }
    }

    fn parse_func_params(&mut self) -> Result<Vec<String>, ParsingError>{
        Ok(Vec::new())
    }

    fn if_stmt(&mut self) -> Result<Statement, ParsingError> {
        self.error_or_match(
            lexer::TokenType::LPARENTHESE,
            "(",
            UnexpectedTokenErrorTypes::LPARENTHESE,
        )?;

        let expr = self.expression()?;

        self.error_or_match(
            lexer::TokenType::RPARENTHESE,
            ")",
            UnexpectedTokenErrorTypes::RPARENTHESE,
        )?;
        self.error_or_match(
            lexer::TokenType::LBRACE,
            "{",
            UnexpectedTokenErrorTypes::LBRACE,
        )?;

        let mut if_stmts = Vec::new();
        while !self
            .lexer
            .try_to_match_token(lexer::TokenType::RBRACE, vec!["}"])
            .0
        {
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
        if self
            .lexer
            .try_to_match_token(lexer::TokenType::KEYWORD, vec!["else"])
            .0
        {
            self.error_or_match(
                lexer::TokenType::LBRACE,
                "{",
                UnexpectedTokenErrorTypes::LBRACE,
            )?;
            while !self
                .lexer
                .try_to_match_token(lexer::TokenType::RBRACE, vec!["}"])
                .0
            {
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
        self.error_or_match(
            lexer::TokenType::SEMICOLON,
            ";",
            UnexpectedTokenErrorTypes::SEMICOLON,
        )?;
        Ok(Statement::Return(ReturnStmt::new(expr)))
    }

    fn var_decl(&mut self, is_const: bool) -> Result<Statement, ParsingError> {
        match self.lexer.match_token(lexer::TokenType::NAME, "") {
            Ok(token) => {
                if self.name_exist_in_current_scope(&token.text) {
                    return Err(ParsingError::ScopeResolution(ScopeResolutionError::new(
                        scope_error_msg_handle(
                            ScopeResolutionErrorTypes::ALREADYDECLARED,
                            &token.text,
                        ),
                        token.line,
                        token.column,
                    )));
                }

                self.error_or_match(
                    lexer::TokenType::COLON,
                    ":",
                    UnexpectedTokenErrorTypes::COLON,
                )?;

                match self.lexer.match_token(lexer::TokenType::TYPE, "") {
                    Ok(token_type) => {
                        self.scopes.create_new_symbol(
                            self.string_to_symbol_type(&token_type.text)?,
                            &token.text,
                            is_const,
                        )?;
                        // var optional initialization
                        let mut expr = None;
                        if self
                            .lexer
                            .try_to_match_token(lexer::TokenType::ATTR, vec!["="])
                            .0
                        {
                            expr = Some(self.expression()?);
                        }
                        self.error_or_match(lexer::TokenType::SEMICOLON, ";", UnexpectedTokenErrorTypes::SEMICOLON)?;
                        Ok(Statement::VarDecl(VarDeclStmt::new(token.text, expr)))
                    }
                    Err(err) => {
                        return Err(ParsingError::UnexpectedToken(UnexpectedTokenError::new(
                            unexpected_token_error_msg(
                                UnexpectedTokenErrorTypes::TYPE,
                                &err.text,
                            ),
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

    fn attr_stmt(&mut self, var_name: String) -> Result<Statement, ParsingError> {
        let expr = self.expression()?;
        self.error_or_match(lexer::TokenType::SEMICOLON, ";", UnexpectedTokenErrorTypes::SEMICOLON)?;
        Ok(Statement::Attr(AttrStmt::new(var_name, expr)))
    }

    // Expression parsing functions
    fn expression(&mut self) -> Result<Expression, ParsingError> {
        self.equality()
    }

    fn equality(&mut self) -> Result<Expression, ParsingError> {
        let mut expr: Expression = self.comparison()?;

        let mut match_result = self
            .lexer
            .try_to_match_token(lexer::TokenType::OP, vec!["==", "!="]);
        while match_result.0 {
            let right = self.comparison()?;
            expr = Expression::Binary(BinaryExpr::new(right, expr, match_result.1.text));
            match_result = self
                .lexer
                .try_to_match_token(lexer::TokenType::OP, vec!["==", "!="]);
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expression, ParsingError> {
        let mut expr = self.term()?;

        let mut match_result = self
            .lexer
            .try_to_match_token(lexer::TokenType::OP, vec![">", ">=", "<", "<="]);
        while match_result.0 {
            let right = self.term()?;
            expr = Expression::Binary(BinaryExpr::new(right, expr, match_result.1.text));
            match_result = self
                .lexer
                .try_to_match_token(lexer::TokenType::OP, vec![">", ">=", "<", "<="])
        }

        Ok(expr)
    }

    fn term(&mut self) -> Result<Expression, ParsingError> {
        let mut expr = self.factor()?;

        let mut match_result = self
            .lexer
            .try_to_match_token(lexer::TokenType::OP, vec!["+", "-"]);
        while match_result.0 {
            let right = self.factor()?;
            expr = Expression::Binary(BinaryExpr::new(right, expr, match_result.1.text));
            match_result = self
                .lexer
                .try_to_match_token(lexer::TokenType::OP, vec!["+", "-"]);
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expression, ParsingError> {
        let mut expr = self.unary()?;

        let mut match_result = self
            .lexer
            .try_to_match_token(lexer::TokenType::OP, vec!["*", "/"]);
        while match_result.0 {
            let right = self.unary()?;
            expr = Expression::Binary(BinaryExpr::new(right, expr, match_result.1.text));
            match_result = self
                .lexer
                .try_to_match_token(lexer::TokenType::OP, vec!["*", "/"]);
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expression, ParsingError> {
        let match_result = self
            .lexer
            .try_to_match_token(lexer::TokenType::OP, vec!["!", "-"]);
        if match_result.0 {
            let right = self.unary()?;
            return Ok(Expression::Unary(UnaryExpr::new(
                right,
                match_result.1.text,
            )));
        }

        self.primary()
    }

    fn primary(&mut self) -> Result<Expression, ParsingError> {
        let mut match_result = self
            .lexer
            .try_to_match_token(lexer::TokenType::INTEGER, vec![]);
        if match_result.0 {
            return Ok(Expression::Literal(LiteralExpr::new(match_result.1.text)));
        }
        match_result = self
            .lexer
            .try_to_match_token(lexer::TokenType::FLOAT, vec![]);
        if match_result.0 {
            return Ok(Expression::Literal(LiteralExpr::new(match_result.1.text)));
        }
        match_result = self
            .lexer
            .try_to_match_token(lexer::TokenType::STRING, vec![]);
        if match_result.0 {
            return Ok(Expression::Literal(LiteralExpr::new(match_result.1.text)));
        }

        match_result = self
            .lexer
            .try_to_match_token(lexer::TokenType::LPARENTHESE, vec!["("]);
        if match_result.0 {
            let expr = self.expression()?;
            self.error_or_match(lexer::TokenType::RPARENTHESE, ")", UnexpectedTokenErrorTypes::RPARENTHESE)?;
            return Ok(Expression::Grouping(GroupingExpr::new(expr)));
        } else {
            return Err(ParsingError::UnexpectedToken(UnexpectedTokenError::new(
                unexpected_token_error_msg(
                    UnexpectedTokenErrorTypes::UNEXPECTEDTOKEN,
                    &match_result.1.text,
                ),
                match_result.1.line,
                match_result.1.column,
            )));
        }
    }

    fn name_exist_in_current_scope(&self, name: &str) -> bool {
        if let Some(_) = self.scopes.find_symbol_in_current_scope(name) {
            return true;
        } else {
            if let Some(_) = self.scopes.find_func_decl_in_current_scope(name) {
                return true;
            } else {
                return false;
            }
        }
    }

    fn error_or_match(
        &mut self,
        token_type: lexer::TokenType,
        token_text: &str,
        error_type: UnexpectedTokenErrorTypes,
    ) -> Result<(), ParsingError> {
        if let Err(err) = self.lexer.match_token(token_type, token_text) {
            return Err(ParsingError::UnexpectedToken(UnexpectedTokenError::new(
                unexpected_token_error_msg(error_type, &err.text),
                err.line,
                err.column,
            )));
        }
        Ok(())
    }

    fn string_to_symbol_type(
        &self,
        type_string: &str,
    ) -> Result<scope_manager::Type, ParsingError> {
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

    #[test]
    fn find_name_in_current_scope_empty_scope() {
        let parser = Parser::new("");

        assert_eq!(parser.name_exist_in_current_scope("var_test"), false);
    }
}
