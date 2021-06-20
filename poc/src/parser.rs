mod lexer;
mod error_msgs;
mod scope_manager;

enum Expression {
    Binary(BinaryExpr),
    Unary(UnaryExpr),
    Literal(LiteralExpr),
    Grouping(GroupingExpr)
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
            operator
        }
    }
}

struct UnaryExpr {
    right: Box<Expression>,
    operator: String
}

impl UnaryExpr {
    pub fn new(right: Expression, operator: String) -> UnaryExpr {
        UnaryExpr{
            right: Box::new(right),
            operator
        }
    }
}

struct LiteralExpr {
    value: String
}

impl LiteralExpr {
    pub fn new(value: String) -> LiteralExpr {
        LiteralExpr{
            value
        }
    }
}

struct GroupingExpr {
    expr: Box<Expression>
}

impl GroupingExpr {
    pub fn new(expr: Expression) -> GroupingExpr {
        GroupingExpr {
            expr: Box::new(expr)
        }
    }
}

trait Statement { 
}

struct If {
   if_stmts: Vec<Box<dyn Statement>>,
   else_stmts: Vec<Box<dyn Statement>>,
   expression: Expression,
}

struct FunctionDeclaration {

}

struct FunctionCall {

}

struct Return {

}

struct Attribution {

}


fn match_token_text(comp_vec: Vec<&str>, token_text: &str) -> bool {
    for text in comp_vec {
        if text == token_text {
            return true
        }
    } 
    false
}

struct Parser<'a>{
    lexer: lexer::Lexer<'a>,
    scopes: scope_manager::ScopeManager
}

impl<'a> Parser<'a> {

    pub fn new(code: &str) -> Parser {
       Parser {
           lexer: lexer::Lexer::new(code),
           scopes: scope_manager::ScopeManager::new()
       }  
    }

    pub fn parse() -> Vec<Box<dyn Statement>> {
        let ast = Vec::new();

        ast
    }

     // Expression parsing functions
    fn expression(&mut self) -> Expression {
        self.equality()
    }

    fn equality(&mut self) -> Expression {
        let mut expr: Expression = self.comparison();
        
        let mut match_result = self.lexer.match_token(lexer::TokenType::OP, vec!["==", "!="]);
        while match_result.0 {
            let right = self.comparison();
            expr = Expression::Binary(BinaryExpr::new(right, expr, match_result.1));
            match_result = self.lexer.match_token(lexer::TokenType::OP, vec!["==", "!="]);
        }

        expr
    }

    fn comparison(&mut self) -> Expression {
        let mut expr = self.term();

        let mut match_result = self.lexer.match_token(lexer::TokenType::OP, vec![">", ">=", "<", "<="]);
        while match_result.0 {
            let right = self.term();
            expr = Expression::Binary(BinaryExpr::new(right, expr, match_result.1));
            match_result = self.lexer.match_token(lexer::TokenType::OP, vec![">", ">=", "<", "<="])
        } 

        expr
    }

    fn term(&mut self) -> Expression {
        let mut expr = self.factor();

        let mut match_result = self.lexer.match_token(lexer::TokenType::OP, vec!["+","-"]);
        while match_result.0 {
            let right = self.factor();
            expr = Expression::Binary(BinaryExpr::new(right, expr, match_result.1));
            match_result = self.lexer.match_token(lexer::TokenType::OP, vec!["+","-"]);
        }

        expr
    }

    fn factor(&mut self) -> Expression {
        let mut expr = self.unary();

        let mut match_result = self.lexer.match_token(lexer::TokenType::OP, vec!["*","/"]);
        while match_result.0 {
            let right = self.unary();
            expr = Expression::Binary(BinaryExpr::new(right, expr, match_result.1));
            let mut match_result = self.lexer.match_token(lexer::TokenType::OP, vec!["*","/"]);
        }

        expr
    }

    fn unary(&mut self) -> Expression {
        let mut match_result = self.lexer.match_token(lexer::TokenType::OP, vec!["!","-"]);
        if match_result.0 {
            let right = self.unary();
            return Expression::Unary(UnaryExpr::new(right, match_result.1));
        }

        self.primary()

    }

    fn primary(&mut self) -> Result<Expression,  {
        let mut match_result = self.lexer.match_token(lexer::TokenType::INTEGER, vec![]);
        if match_result.0 {
            return Expression::Literal(LiteralExpr::new(match_result.1));
        }
        match_result = self.lexer.match_token(lexer::TokenType::FLOAT, vec![]);
        if match_result.0 {
            return Expression::Literal(LiteralExpr::new(match_result.1));
        }
        match_result = self.lexer.match_token(lexer::TokenType::STRING, vec![]);
        if match_result.0 {
            return Expression::Literal(LiteralExpr::new(match_result.1));
        }

        match_result = self.lexer.match_token(lexer::TokenType::LPARENTHESE, vec!["("]);
        if match_result.0 {
            let expr = self.expression();
            match_result = self.lexer.match_token(lexer::TokenType::RPARENTHESE, vec![")"]);
            return Expression::Grouping(GroupingExpr::new(expr));
        } else {
            return Err
        }


    }

    fn find_name_in_current_scope(& self, name: &str) ->  Option<ScopeTypes> {
        if let Some(symbol) = self.scopes.find_symbol_in_current_scope(name){
            return Some(ScopeTypes::Symbol(symbol.clone()));
        } else {
            if let Some(func_decl) = self.scopes.find_func_decl_in_current_scope(name) {
               return Some(ScopeTypes::FuncDecl(func_decl.clone())); 
            } else {
                return None
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn find_name_in_current_scope_empty_scope() {
        let parser = Parser::new("");

        if let Some(_) = parser.find_name_in_current_scope("var_test"){
            assert!(false);
        } else {
            assert!(true);
        }
    }

    #[test]
    fn match_token_text_test() {
        assert!(match_token_text(vec!("==", "!="), "=="));
        assert!(!match_token_text(vec!("==", "!="), "+"));
    }
}
