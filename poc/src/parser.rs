mod lexer;
mod error_msgs;
mod scope_manager;

#[derive(PartialEq, Clone)]
pub enum NodeType {
    ATTR,
    EXPR,
    ROOT,
    PRIMARY,
    BINARYOP,
    UNARYOP,
    FUNCDECL,
    VARDECL
}

#[derive(Clone)]
pub struct ParsingNode {
    pub node_type: NodeType,
    pub text: String,
}

impl ParsingNode {
    pub fn new(node_type: NodeType, text: String) -> ParsingNode {
        ParsingNode { 
            node_type, 
            text 
        }
    }
}

impl std::cmp::PartialEq for ParsingNode {
    fn eq(&self, other: &Self) -> bool {
        self.text == other.text && self.node_type == other.node_type
    }
}

struct AstNode {
    node: ParsingNode,
    childrens: Vec<AstNode>
}

impl AstNode {
    fn new(node_type: NodeType, text: String) -> AstNode{
        AstNode {
            node: ParsingNode::new(node_type, text),
            childrens: Vec::new()
        }
    }

    fn new_with_children(node_type: NodeType, text: String, right: AstNode, left: AstNode) -> AstNode {
       let mut node = AstNode::new(node_type, text);
       node.insert_children(left);
       node.insert_children(right);
       node
    }

    fn insert_children(&mut self, node: AstNode) {
       self.childrens.push(node) 
    }
}

// Used only as return type of function find_name_in_current_scope 
enum ScopeTypes {
    Symbol(scope_manager::Symbol),
    FuncDecl(scope_manager::FuncDecl)
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

    pub fn build_ast(&mut self) -> AstNode {
        let mut root = AstNode {
            node: ParsingNode::new(NodeType::ROOT, String::from("ROOT")),
            childrens: Vec::new(),
        }; 
        self.parse(&mut root);
        root
    }

    fn parse(&mut self, root: &mut AstNode) -> Result<(), String> {
       
        let mut lookahead = self.lexer.peek_token();  
        match lookahead.token_type {
            lexer::TokenType::KEYWORD => {
                match lookahead.text.as_str() {
                    "function" => {
                        match self.parse_function_decl() {
                            Ok(_) => (),
                            Err(e) => return Err(e)
                        }
                    }
                    "let" | "const" => {
                        let is_const = match lookahead.text.as_str() {
                            "const" => true,
                            _ => false
                        };
                        match self.parse_var_decl(is_const) {
                            Ok(_) => (),
                            Err(e) => return Err(e)
                        }
                    }
                    "if" => {
                        // TODO: if statement
                    }
                    "else" => {
                        // TODO: else statement
                    }
                    "return" => {
                        // TODO: return statement
                    }
                    _ => {
                        return Err(error_msgs::parser::wrong_token_error_msg_handle(
                            error_msgs::parser::UnexpectedTokenError::UNEXPECTEDKEYWORD, 
                            &lookahead.text
                            ));
                    }
                }
            }
            lexer::TokenType::NAME => {
                let name = lookahead.text;
                lookahead = self.lexer.peek_token();
                if lookahead.token_type == lexer::TokenType::ATTR {
                    self.expression();
                } else if lookahead.token_type == lexer::TokenType::LPARENTHESE {
                    self.parse_function_call();
                } else {
                    return Err(error_msgs::parser::wrong_token_error_msg_handle(
                        error_msgs::parser::UnexpectedTokenError::UNEXPECTEDTOKEN, 
                        &lookahead.text
                        ));
                }
            },
            _ => {
                return Err(error_msgs::parser::wrong_token_error_msg_handle(
                    error_msgs::parser::UnexpectedTokenError::UNEXPECTEDTOKEN, 
                    &lookahead.text
                    ));
            }
        }

        // End of parse function
        Ok(())
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

    fn parse_function_decl(&mut self) -> Result<AstNode, String> {
        let mut lookahead = self.lexer.peek_token();
        if lookahead.token_type != lexer::TokenType::NAME {
            return Err(error_msgs::parser::wrong_token_error_msg_handle(
                    error_msgs::parser::UnexpectedTokenError::FUNCNAME, 
                    &lookahead.text
                    ));
        } 
        let function_name = lookahead.text;

        lookahead = self.lexer.peek_token();
        if lookahead.token_type != lexer::TokenType::LPARENTHESE {
            return Err(error_msgs::parser::wrong_token_error_msg_handle(
                    error_msgs::parser::UnexpectedTokenError::LPARENTHESE, 
                    &lookahead.text
                    ));
        }

        lookahead = self.lexer.peek_token();
        loop {
            if lookahead.token_type != lexer::TokenType::NAME {
                return Err(error_msgs::parser::wrong_token_error_msg_handle(
                    error_msgs::parser::UnexpectedTokenError::FUNCNAME, 
                    &lookahead.text
                    ));
            }    
            lookahead = self.lexer.peek_token();
            
            if lookahead.token_type != lexer::TokenType::COLON {
                return Err(error_msgs::parser::wrong_token_error_msg_handle(
                    error_msgs::parser::UnexpectedTokenError::COLON, 
                    &lookahead.text
                    ));
            }    
            
            lookahead = self.lexer.peek_token();
            
            if lookahead.token_type != lexer::TokenType::TYPE {
                return Err(error_msgs::parser::wrong_token_error_msg_handle(
                    error_msgs::parser::UnexpectedTokenError::TYPE, 
                    &lookahead.text
                    ));
            }    

            match lookahead.token_type {
                lexer::TokenType::RPARENTHESE => break,
                lexer::TokenType::COMMA => (),
                _ => return Err(error_msgs::parser::wrong_token_error_msg_handle(
                    error_msgs::parser::UnexpectedTokenError::COMMAORRPARENTHESE, 
                    &lookahead.text
                    ))
            };
        } 

        Ok(AstNode::new(NodeType::FUNCDECL, function_name))
        // TODO: Finish function parsing implementation
    }

    fn parse_var_decl(&mut self, is_const: bool) -> Result<AstNode, String> { 

        let mut lookahead = self.lexer.peek_token(); 
        if lookahead.token_type != lexer::TokenType::NAME { 
            return Err(error_msgs::parser::wrong_token_error_msg_handle(
                error_msgs::parser::UnexpectedTokenError::VARNAME, 
                &lookahead.text
                ));
        }

        let var_name = lookahead.text;
        match self.find_name_in_current_scope(&var_name) {
            Some(_) => return Err(error_msgs::parser::scope_error_msg_handle(
                    error_msgs::parser::ScopeError::ALREADYDECLARED,
                    &var_name)
                ),
            None => ()

        }
        
        lookahead = self.lexer.peek_token();
        if lookahead.token_type != lexer::TokenType::COLON {
                return Err(error_msgs::parser::wrong_token_error_msg_handle(
                    error_msgs::parser::UnexpectedTokenError::COLON, 
                    &lookahead.text
                    ));
        }

        lookahead = self.lexer.peek_token();
        if lookahead.token_type != lexer::TokenType::TYPE {
                return Err(error_msgs::parser::wrong_token_error_msg_handle(
                    error_msgs::parser::UnexpectedTokenError::TYPE, 
                    &lookahead.text
                    ));
        }

        let var_type = lookahead.text;
        let new_symbol = match scope_manager::Symbol::new_by_string(&var_type, is_const){
            Ok(x) => x,
            Err(e) => {
                return Err(error_msgs::parser::internal_error_msg_handle(
                        error_msgs::parser::InternalError::UNABLETOCREATESYMBOL, 
                        &e
                        ));
            }
        };

        match self.scopes.insert_symbol(new_symbol, var_name.clone()) {
            Ok(_) => (),
            Err(e) => {
                return Err(error_msgs::parser::internal_error_msg_handle(
                        error_msgs::parser::InternalError::UNABLETOINSERTSYMBOL, 
                        e
                        ));
            }
        }
        
        lookahead = self.lexer.peek_token();
        if lookahead.token_type != lexer::TokenType::SEMICOLON {
            return Err(error_msgs::parser::wrong_token_error_msg_handle(
                error_msgs::parser::UnexpectedTokenError::SEMICOLON, 
                &lookahead.text
                ));
        }

        Ok(AstNode::new(NodeType::VARDECL, var_name))
        // TODO: Implement attribution with variable declaration

    }
     
    // Expression parsing functions
    fn expression(&mut self) -> AstNode {
        self.equality()
    }

    fn equality(&mut self) -> AstNode {
        let mut expr = self.comparison();

        let mut lookahead = self.lexer.peek_token();
        while match_token_text(vec!["==", "!="], &lookahead.text) {
            let right = self.comparison();
            expr = AstNode::new_with_children(NodeType::BINARYOP, lookahead.text, right, expr);
            lookahead = self.lexer.peek_token();
        } 

        expr
    }

    fn comparison(&mut self)-> AstNode {
        let mut expr = self.term();

        let mut lookahead = self.lexer.peek_token();
        while match_token_text(vec![">", ">=", "<", "<="], &lookahead.text) {
            let right = self.term();
            expr = AstNode::new_with_children(NodeType::BINARYOP, lookahead.text, right, expr);
            lookahead = self.lexer.peek_token();
        } 

        expr
    }

    fn term(&mut self) -> AstNode {
        let mut expr = self.factor();

        let mut lookahead = self.lexer.peek_token();
        while match_token_text(vec!["+", "-"], &lookahead.text) {
            let right = self.factor();
            expr = AstNode::new_with_children(NodeType::BINARYOP, lookahead.text, right, expr);
            lookahead = self.lexer.peek_token();
        }

        expr
    }

    fn factor(&mut self) -> AstNode {
        let mut expr = self.unary();

        let mut lookahead = self.lexer.peek_token();
        while match_token_text(vec!["*", "/"], &lookahead.text) {
            let right = self.unary();
            expr = AstNode::new_with_children(NodeType::BINARYOP, lookahead.text, right, expr);
            lookahead = self.lexer.peek_token();
        }

        expr
    }

    fn unary(&mut self) -> AstNode {
        let lookahead = self.lexer.peek_token();
        if match_token_text(vec!["!", "/"], &lookahead.text) {
            let mut expr = AstNode::new(NodeType::UNARYOP, lookahead.text);
            let right = self.unary();
            expr.insert_children(right);
            return expr;
        }

        self.primary()

    }

    fn primary(&mut self) -> AstNode {
        AstNode::new(NodeType::PRIMARY, String::from(""))
    }

    fn parse_function_call(&mut self) {
        // TODO
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
