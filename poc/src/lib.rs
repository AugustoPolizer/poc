mod lexer {

    pub struct Lexer<'a> {
        code_iterator: std::iter::Peekable<std::str::Chars<'a>>,
        keywords: std::collections::HashSet<&'static str>,
        types: std::collections::HashSet<&'static str>,
        peeked_tokens: std::collections::VecDeque<Token>
    }

    #[derive(PartialEq, Debug, Clone)]
    pub enum TokenType {
        NAME,
        KEYWORD,
        TYPE,
        INTEGER,
        FLOAT,
        STRING,
        ATTR,
        OP,
        ARROW,
        UNARYOP,
        LPARENTHESE,
        RPARENTHESE,
        LBRACK,
        RBRACK,
        LBRACE,
        RBRACE,
        SEMICOLON,
        COLON,
        COMMA,
        EOF,
        ROOT,
        UNKNOWN
    }

    #[derive(Clone)]
    pub struct Token {
        pub token_type: TokenType,
        pub text: String,
    }

    impl Token {
        pub fn new(token_type: TokenType, text: String) -> Token {
            Token { token_type, text }
        }
    }

    impl std::cmp::PartialEq for Token {
        fn eq(&self, other: &Self) -> bool {
            self.text == other.text && self.token_type == other.token_type
        }
    }

    impl<'a> Lexer<'a> {
        pub fn new(raw_code: &'a str) -> Lexer<'a> {
            Lexer {
                code_iterator: raw_code.chars().peekable(),
                keywords: ["function", "return", "if", "else", "let", "const"].iter().cloned().collect(),
                types: ["int", "float", "void", "string"].iter().cloned().collect(),
                peeked_tokens: std::collections::VecDeque::new()
            }
        }

        pub fn consume_token(&mut self) -> Token {
            let token = match self.peeked_tokens.pop_front() {
                Some(t) => t,
                None => self.get_token()
            };
            token
        }

        pub fn peek_token(&mut self) -> Token {
            let token = self.get_token();
            self.peeked_tokens.push_back(token.clone());

            token
        }

        fn get_token(&mut self) -> Token {

            let mut lookahead = match self.code_iterator.peek() {
                Some(c) => *c,
                None => return Token::new(TokenType::EOF, String::from("EOF"))
            };

            // Remove whitespaces 
            if lookahead == ' ' || lookahead == '\n' || lookahead == '\t' || lookahead == '\r' {
                self.code_iterator.next();
                loop {
                    lookahead = match self.code_iterator.peek() {
                        Some(c) => *c,
                        None => return Token::new(TokenType::EOF, String::from("EOF")),
                    };
                    match lookahead {
                        ' ' | '\n' | '\t' | '\r' => self.code_iterator.next(),
                        _ => break,
                    };
                }
            }

            // Faz o parsing de tokens que precisando de um lookahead igual a 2
            let mut parsing_tokens_ll2 = | second_possible_char: char, first_token_type: TokenType, second_token_type: TokenType| -> Token {
                let mut buffer = String::new();
                buffer.push(lookahead);
                self.code_iterator.next();
                let lookahead_2 = match self.code_iterator.peek() {
                    Some(c) => *c,
                    None => return Token::new(first_token_type, buffer)
                };
                if lookahead_2 != second_possible_char {
                    return Token::new(first_token_type, buffer);
                }
                buffer.push(lookahead_2);
                self.code_iterator.next();
                Token::new(second_token_type, buffer)
            };

            let result = match lookahead {
                'a'..='z' | 'A'..='Z' => {
                    let mut buffer = String::new();
                    loop {
                        buffer.push(lookahead);
                        self.code_iterator.next();
                        lookahead = match self.code_iterator.peek() {
                            Some(c) => *c,
                            None => break,
                        };
                        if !lookahead.is_ascii_alphabetic() && !(lookahead == '_') {
                            break;
                        }
                    }
                    if self.keywords.contains(buffer.as_str()) {
                        return Token::new(TokenType::KEYWORD, buffer);
                    } else if self.types.contains(buffer.as_str()){
                        return Token::new(TokenType::TYPE, buffer);
                    }
                    Token::new(TokenType::NAME, buffer)
                }
                '"' => {
                    let mut buffer = String::new();
                    loop {
                        self.code_iterator.next();
                        lookahead = match self.code_iterator.peek() {
                            Some(c) => *c,
                            None => break,
                        };
                        if lookahead == '"' {
                            self.code_iterator.next();
                            break;
                        }
                        buffer.push(lookahead);
                    }
                    Token::new(TokenType::STRING, buffer)
                }
                '0'..='9' => {
                    let mut float = false;
                    let mut buffer = String::new();
                    loop {
                        buffer.push(lookahead);
                        self.code_iterator.next();
                        lookahead = match self.code_iterator.peek() {
                            Some(c) => *c,
                            None => break,
                        };
                        if !lookahead.is_ascii_digit() {
                            if lookahead == '.' && float == false {
                                float = true;
                            } else {
                                break;
                            }
                        }
                    }
                    if float {
                        return Token::new(TokenType::FLOAT, buffer) 
                    }
                    Token::new(TokenType::INTEGER, buffer) 
                }
                '&' => {
                    parsing_tokens_ll2('&', TokenType::OP, TokenType::OP)
                }
                '|' => {
                    parsing_tokens_ll2('|', TokenType::OP, TokenType::OP)
                }
                '<' | '>' =>  {
                    parsing_tokens_ll2('=', TokenType::OP, TokenType::OP)
                }
                '=' => {
                    parsing_tokens_ll2('=', TokenType::ATTR, TokenType::OP)
                }
                '-' => {
                    parsing_tokens_ll2('>', TokenType::OP, TokenType::ARROW)
                }
                '!' => {
                    parsing_tokens_ll2('=', TokenType::UNARYOP, TokenType::OP)
                }
                '+' | '*' | '/' => {
                    self.code_iterator.next();
                   Token::new(TokenType::OP, String::from(lookahead))
                }
                ';' => {
                    self.code_iterator.next();
                    Token::new(TokenType::SEMICOLON, String::from(";")) 
                }
                ',' => {
                    self.code_iterator.next();
                    Token::new(TokenType::COMMA, String::from(",")) 
                }
                ':' => {
                    self.code_iterator.next();
                    Token::new(TokenType::COLON, String::from(":")) 
                }
                '(' => {
                    self.code_iterator.next();
                    Token::new(TokenType::LPARENTHESE, String::from("(")) 
                }
                ')' => {
                    self.code_iterator.next();
                    Token::new(TokenType::RPARENTHESE, String::from(")")) 
                }
                '[' => {
                    self.code_iterator.next();
                    Token::new(TokenType::LBRACK, String::from("[")) 
                }
                ']' => {
                    self.code_iterator.next();
                    Token::new(TokenType::RBRACK, String::from("]")) 
                }
                '{' => {
                    self.code_iterator.next();
                    Token::new(TokenType::LBRACE, String::from("{")) 
                }
                '}' => {
                    self.code_iterator.next();
                    Token::new(TokenType::RBRACE, String::from("}")) 
                }
                _ => {
                    self.code_iterator.next();
                    Token::new(TokenType::UNKNOWN, String::from("UNK"))
                }
            };

            result
        }

    }

    #[cfg(test)]
    mod tests {
        use super::*;

        fn assert_token_equal(lexer: &mut Lexer, text: &str, token_type: TokenType){
            let token = lexer.get_token();
            assert_eq!(token.text, text);
            assert_eq!(token.token_type, token_type);
        }

        #[test]
        fn get_token_simple_function() {
            let code = r"
                function simple_function() -> int {
                    return 1;
                }
                ";
            let mut lexer: Lexer = Lexer::new(&code);

            assert_token_equal(&mut lexer, "function", TokenType::KEYWORD);
            assert_token_equal(&mut lexer, "simple_function", TokenType::NAME);
            assert_token_equal(&mut lexer, "(", TokenType::LPARENTHESE);
            assert_token_equal(&mut lexer, ")", TokenType::RPARENTHESE);
            assert_token_equal(&mut lexer, "->", TokenType::ARROW);
            assert_token_equal(&mut lexer, "int", TokenType::TYPE);
            assert_token_equal(&mut lexer, "{", TokenType::LBRACE);
            assert_token_equal(&mut lexer, "return", TokenType::KEYWORD);
            assert_token_equal(&mut lexer, "1", TokenType::INTEGER);
            assert_token_equal(&mut lexer, ";", TokenType::SEMICOLON);
            assert_token_equal(&mut lexer, "}", TokenType::RBRACE);
            assert_token_equal(&mut lexer, "EOF", TokenType::EOF);
        }

        #[test]
        fn get_token_attribution() {
            let code = r"
            function attributionTest() -> void {
                const a : int = 1;
                const b : float = 3.14;
                let c : float = a + b;
            }";
            let mut lexer: Lexer = Lexer::new(&code);
            
            assert_token_equal(&mut lexer, "function", TokenType::KEYWORD);
            assert_token_equal(&mut lexer, "attributionTest", TokenType::NAME);
            assert_token_equal(&mut lexer, "(", TokenType::LPARENTHESE);
            assert_token_equal(&mut lexer, ")", TokenType::RPARENTHESE);
            assert_token_equal(&mut lexer, "->", TokenType::ARROW);
            assert_token_equal(&mut lexer, "void", TokenType::TYPE);
            assert_token_equal(&mut lexer, "{", TokenType::LBRACE);
            assert_token_equal(&mut lexer, "const", TokenType::KEYWORD);
            assert_token_equal(&mut lexer, "a", TokenType::NAME);
            assert_token_equal(&mut lexer, ":", TokenType::COLON);
            assert_token_equal(&mut lexer, "int", TokenType::TYPE);
            assert_token_equal(&mut lexer, "=", TokenType::ATTR);
            assert_token_equal(&mut lexer, "1", TokenType::INTEGER);
            assert_token_equal(&mut lexer, ";", TokenType::SEMICOLON);
            assert_token_equal(&mut lexer, "const", TokenType::KEYWORD);
            assert_token_equal(&mut lexer, "b", TokenType::NAME);
            assert_token_equal(&mut lexer, ":", TokenType::COLON);
            assert_token_equal(&mut lexer, "float", TokenType::TYPE);
            assert_token_equal(&mut lexer, "=", TokenType::ATTR);
            assert_token_equal(&mut lexer, "3.14", TokenType::FLOAT);
            assert_token_equal(&mut lexer, ";", TokenType::SEMICOLON);
            assert_token_equal(&mut lexer, "let", TokenType::KEYWORD);
            assert_token_equal(&mut lexer, "c", TokenType::NAME);
            assert_token_equal(&mut lexer, ":", TokenType::COLON);
            assert_token_equal(&mut lexer, "float", TokenType::TYPE);
            assert_token_equal(&mut lexer, "=", TokenType::ATTR);
            assert_token_equal(&mut lexer, "a", TokenType::NAME);
            assert_token_equal(&mut lexer, "+", TokenType::OP);
            assert_token_equal(&mut lexer, "b", TokenType::NAME);
            assert_token_equal(&mut lexer, ";", TokenType::SEMICOLON);
            assert_token_equal(&mut lexer, "}", TokenType::RBRACE);
            assert_token_equal(&mut lexer, "EOF", TokenType::EOF);
        }

        #[test]
        fn get_token_binary_op() {
            let code = r"
                function operators ( ) -> float {
                    return (500.01 + 3) * 2.015 + (1 / 3);
                }
                ";
            
            let mut lexer: Lexer = Lexer::new(&code);
           
            assert_token_equal(&mut lexer, "function", TokenType::KEYWORD);
            assert_token_equal(&mut lexer, "operators", TokenType::NAME);
            assert_token_equal(&mut lexer, "(", TokenType::LPARENTHESE);
            assert_token_equal(&mut lexer, ")", TokenType::RPARENTHESE);
            assert_token_equal(&mut lexer, "->", TokenType::ARROW);
            assert_token_equal(&mut lexer, "float", TokenType::TYPE);
            assert_token_equal(&mut lexer, "{", TokenType::LBRACE);
            assert_token_equal(&mut lexer, "return", TokenType::KEYWORD);
            assert_token_equal(&mut lexer, "(", TokenType::LPARENTHESE);
            assert_token_equal(&mut lexer, "500.01", TokenType::FLOAT);
            assert_token_equal(&mut lexer, "+", TokenType::OP);
            assert_token_equal(&mut lexer, "3", TokenType::INTEGER);
            assert_token_equal(&mut lexer, ")", TokenType::RPARENTHESE);
            assert_token_equal(&mut lexer, "*", TokenType::OP);
            assert_token_equal(&mut lexer, "2.015", TokenType::FLOAT);
            assert_token_equal(&mut lexer, "+", TokenType::OP);
            assert_token_equal(&mut lexer, "(", TokenType::LPARENTHESE);
            assert_token_equal(&mut lexer, "1", TokenType::INTEGER);
            assert_token_equal(&mut lexer, "/", TokenType::OP);
            assert_token_equal(&mut lexer, "3", TokenType::INTEGER);
            assert_token_equal(&mut lexer, ")", TokenType::RPARENTHESE);
            assert_token_equal(&mut lexer, ";", TokenType::SEMICOLON);
            assert_token_equal(&mut lexer, "}", TokenType::RBRACE);
            assert_token_equal(&mut lexer, "EOF", TokenType::EOF);
        }

        #[test]
        fn get_token_string() {
            let code = r#"
                let nome: string = "Augusto";
                const nome_completo: string = "Augusto Polizer";
                "#;

            let mut lexer: Lexer = Lexer::new(&code);

            assert_token_equal(&mut lexer, "let", TokenType::KEYWORD);
            assert_token_equal(&mut lexer, "nome", TokenType::NAME);
            assert_token_equal(&mut lexer, ":", TokenType::COLON);
            assert_token_equal(&mut lexer, "string", TokenType::TYPE);
            assert_token_equal(&mut lexer, "=", TokenType::ATTR);
            assert_token_equal(&mut lexer, "Augusto", TokenType::STRING);
            assert_token_equal(&mut lexer, ";", TokenType::SEMICOLON);
            assert_token_equal(&mut lexer, "const", TokenType::KEYWORD);
            assert_token_equal(&mut lexer, "nome_completo", TokenType::NAME);
            assert_token_equal(&mut lexer, ":", TokenType::COLON);
            assert_token_equal(&mut lexer, "string", TokenType::TYPE);
            assert_token_equal(&mut lexer, "=", TokenType::ATTR);
            assert_token_equal(&mut lexer, "Augusto Polizer", TokenType::STRING);
            assert_token_equal(&mut lexer, ";", TokenType::SEMICOLON);
        }

        #[test]
        fn get_token_if_else() {
            let code = r"
                function operators(x: int, limit: int) -> float {
                    if (x <= 10 && x >= 0) {
                        return 10;
                    } else if (x <= 100) {
                        return 100;
                    } else {
                        return limit; 
                    }
                }
                ";
            
            let mut lexer: Lexer = Lexer::new(&code);
            assert_token_equal(&mut lexer, "function", TokenType::KEYWORD);
            assert_token_equal(&mut lexer, "operators", TokenType::NAME);
            assert_token_equal(&mut lexer, "(", TokenType::LPARENTHESE);
            assert_token_equal(&mut lexer, "x", TokenType::NAME);
            assert_token_equal(&mut lexer, ":", TokenType::COLON);
            assert_token_equal(&mut lexer, "int", TokenType::TYPE);
            assert_token_equal(&mut lexer, ",", TokenType::COMMA);
            assert_token_equal(&mut lexer, "limit", TokenType::NAME);
            assert_token_equal(&mut lexer, ":", TokenType::COLON);
            assert_token_equal(&mut lexer, "int", TokenType::TYPE);
            assert_token_equal(&mut lexer, ")", TokenType::RPARENTHESE);
            assert_token_equal(&mut lexer, "->", TokenType::ARROW);
            assert_token_equal(&mut lexer, "float", TokenType::TYPE);
            assert_token_equal(&mut lexer, "{", TokenType::LBRACE);
            assert_token_equal(&mut lexer, "if", TokenType::KEYWORD);
            assert_token_equal(&mut lexer, "(", TokenType::LPARENTHESE);
            assert_token_equal(&mut lexer, "x", TokenType::NAME);
            assert_token_equal(&mut lexer, "<=", TokenType::OP);
            assert_token_equal(&mut lexer, "10", TokenType::INTEGER);
            assert_token_equal(&mut lexer, "&&", TokenType::OP);
            assert_token_equal(&mut lexer, "x", TokenType::NAME);
            assert_token_equal(&mut lexer, ">=", TokenType::OP);
            assert_token_equal(&mut lexer, "0", TokenType::INTEGER);
            assert_token_equal(&mut lexer, ")", TokenType::RPARENTHESE);
            assert_token_equal(&mut lexer, "{", TokenType::LBRACE);
            assert_token_equal(&mut lexer, "return", TokenType::KEYWORD);
            assert_token_equal(&mut lexer, "10", TokenType::INTEGER);
            assert_token_equal(&mut lexer, ";", TokenType::SEMICOLON);
            assert_token_equal(&mut lexer, "}", TokenType::RBRACE);
            assert_token_equal(&mut lexer, "else", TokenType::KEYWORD);
            assert_token_equal(&mut lexer, "if", TokenType::KEYWORD);
            assert_token_equal(&mut lexer, "(", TokenType::LPARENTHESE);
            assert_token_equal(&mut lexer, "x", TokenType::NAME);
            assert_token_equal(&mut lexer, "<=", TokenType::OP);
            assert_token_equal(&mut lexer, "100", TokenType::INTEGER);
            assert_token_equal(&mut lexer, ")", TokenType::RPARENTHESE);
            assert_token_equal(&mut lexer, "{", TokenType::LBRACE);
            assert_token_equal(&mut lexer, "return", TokenType::KEYWORD);
            assert_token_equal(&mut lexer, "100", TokenType::INTEGER);
            assert_token_equal(&mut lexer, ";", TokenType::SEMICOLON);
            assert_token_equal(&mut lexer, "}", TokenType::RBRACE);
            assert_token_equal(&mut lexer, "else", TokenType::KEYWORD);
            assert_token_equal(&mut lexer, "{", TokenType::LBRACE);
            assert_token_equal(&mut lexer, "return", TokenType::KEYWORD);
            assert_token_equal(&mut lexer, "limit", TokenType::NAME);
            assert_token_equal(&mut lexer, ";", TokenType::SEMICOLON);
            assert_token_equal(&mut lexer, "}", TokenType::RBRACE);
            assert_token_equal(&mut lexer, "}", TokenType::RBRACE);
            assert_token_equal(&mut lexer, "EOF", TokenType::EOF);
        }

        fn assert_token_equal_peek(lexer: &mut Lexer, text: &str, token_type: TokenType){
            let token = lexer.peek_token();
            assert_eq!(token.text, text);
            assert_eq!(token.token_type, token_type);
        }

        #[test]
        fn peek() {
            let code = r"
                let a: int = 10; 
                ";
            let mut lexer: Lexer = Lexer::new(&code);
            
            assert_token_equal_peek(&mut lexer, "let", TokenType::KEYWORD);    
            assert_token_equal_peek(&mut lexer, "a", TokenType::NAME);    
            assert_token_equal_peek(&mut lexer, ":", TokenType::COLON);    
            assert_token_equal_peek(&mut lexer, "int", TokenType::TYPE);    
            assert_token_equal_peek(&mut lexer, "=", TokenType::ATTR);    
            assert_token_equal_peek(&mut lexer, "10", TokenType::INTEGER);    
            assert_token_equal_peek(&mut lexer, ";", TokenType::SEMICOLON);    
            assert_token_equal_peek(&mut lexer, "EOF", TokenType::EOF);    
            assert_token_equal_peek(&mut lexer, "EOF", TokenType::EOF);    
        }
        
        fn assert_token_equal_consume(lexer: &mut Lexer, text: &str, token_type: TokenType){
            let token = lexer.consume_token();
            assert_eq!(token.text, text);
            assert_eq!(token.token_type, token_type);
        }

        #[test]
        fn consume() {
            let code = r"
                let a: int = 10; 
                ";
            let mut lexer: Lexer = Lexer::new(&code);
            
            assert_token_equal_consume(&mut lexer, "let", TokenType::KEYWORD);    
            assert_token_equal_consume(&mut lexer, "a", TokenType::NAME);    
            assert_token_equal_consume(&mut lexer, ":", TokenType::COLON);    
            assert_token_equal_consume(&mut lexer, "int", TokenType::TYPE);    
            assert_token_equal_consume(&mut lexer, "=", TokenType::ATTR);    
            assert_token_equal_consume(&mut lexer, "10", TokenType::INTEGER);    
            assert_token_equal_consume(&mut lexer, ";", TokenType::SEMICOLON);    
            assert_token_equal_consume(&mut lexer, "EOF", TokenType::EOF);    
            assert_token_equal_consume(&mut lexer, "EOF", TokenType::EOF);    
        }
        
        #[test]
        fn consume_and_peek() {
            let code = r"
                let a: int = 10; 
                ";
            let mut lexer: Lexer = Lexer::new(&code);
            
            assert_token_equal_consume(&mut lexer, "let", TokenType::KEYWORD);    
            assert_token_equal_peek(&mut lexer, "a", TokenType::NAME);    
            assert_token_equal_peek(&mut lexer, ":", TokenType::COLON);    
            assert_token_equal_consume(&mut lexer, "a", TokenType::NAME);    
            assert_token_equal_consume(&mut lexer, ":", TokenType::COLON);    
            assert_token_equal_consume(&mut lexer, "int", TokenType::TYPE);     
            assert_token_equal_peek(&mut lexer, "=", TokenType::ATTR);    
            assert_token_equal_peek(&mut lexer, "10", TokenType::INTEGER);    
            assert_token_equal_peek(&mut lexer, ";", TokenType::SEMICOLON);    
            assert_token_equal_peek(&mut lexer, "EOF", TokenType::EOF);    
            assert_token_equal_peek(&mut lexer, "EOF", TokenType::EOF);     
            assert_token_equal_consume(&mut lexer, "=", TokenType::ATTR);    
            assert_token_equal_consume(&mut lexer, "10", TokenType::INTEGER);    
            assert_token_equal_consume(&mut lexer, ";", TokenType::SEMICOLON);    
            assert_token_equal_consume(&mut lexer, "EOF", TokenType::EOF);    
            assert_token_equal_consume(&mut lexer, "EOF", TokenType::EOF);    
        }
    }
}


mod scope_manager{
    
    use std::collections;
    
    #[derive(PartialEq, Debug, Clone)]
    pub enum Type {
        INTEGER,
        FLOAT,
        STRING
    }

    struct Symbol {
        symbol_type: Type
    }

    impl Symbol {
        fn new(symbol_type: Type) -> Symbol {
            Symbol{
                symbol_type
            }
        }
    }

    struct FuncDecl {
        return_type: Type,
        params: Vec<Symbol>
    }

    impl FuncDecl {
        fn new(return_type: Type, params: Vec<Symbol>) -> FuncDecl {
            FuncDecl {
                return_type,
                params
            }
        }
    }

    struct Scope {
        symbol_table: collections::HashMap<String, Symbol>,
        function_table: collections::HashMap<String, FuncDecl>,
    }

    impl Scope {
        fn new() -> Scope {
            Scope {
                symbol_table: collections::HashMap::new(),
                function_table: collections::HashMap::new(),
            }
        }
    }
    
    struct ScopeManager {
        scopes: Vec<Scope>
    }

    impl ScopeManager {
        pub fn new() -> ScopeManager {
            ScopeManager {
                scopes: Vec::new(),
            }
        }

        pub fn create_new_scope(&mut self) {
           self.scopes.push(Scope::new()); 
        }

        pub fn remove_scope(&mut self) -> Option<Scope> {
            match self.scopes.pop() {
                Some(s) => Some(s),
                None => None
            }
        }

        pub fn insert_symbol(& mut self, symbol: Symbol, symbol_name: String) -> Result<(), &'static str> {
           if self.scopes.is_empty() {
               return Err("There is no scope to insert symbol");
           }
           let idx = self.scopes.len() - 1;
           self.scopes[idx].symbol_table.insert(symbol_name, symbol);

           Ok(())
        }

        pub fn insert_func_decl(& mut self, func_decl: FuncDecl, func_name: String) -> Result<(), &'static str> {
           if self.scopes.is_empty() {
               return Err("There is no scope to insert symbol");
           }
           let idx = self.scopes.len() - 1;
           self.scopes[idx].function_table.insert(func_name, func_decl);

           Ok(())
        }

        pub fn find_symbol(&mut self, name: &str) -> Option<&Symbol> {
            let iter = self.scopes.iter().rev();
            for scope in iter {
                match scope.symbol_table.get(name) {
                    Some(s) => return Some(s),
                    None => () 
                };
            }
            None
        }
        
        pub fn find_func_decl(&mut self, name: &str) -> Option<& FuncDecl> {
            let iter = self.scopes.iter().rev();
            for scope in iter {
                match scope.function_table.get(name) {
                    Some(f) => return Some(f),
                    None => () 
                };
            }
            None
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn create_scope() {
            let mut scope_manager = ScopeManager::new();

            scope_manager.create_new_scope();
            match scope_manager.remove_scope() {
                Some(_) => assert!(true),
                None => assert!(false)
            };
        }


        #[test]
        fn insert_symbol_correct_use() {
            let mut scope_manager = ScopeManager::new();

            scope_manager.create_new_scope();
            match scope_manager.insert_symbol(Symbol::new(Type::INTEGER), String::from("var_test")) {
                Ok(_) => assert!(true),
                Err(_) => assert!(false)
            }
        } 

        #[test]
        fn insert_symbol_incorrect_use() {
            let mut scope_manager = ScopeManager::new();

            match scope_manager.insert_symbol(Symbol::new(Type::INTEGER), String::from("var_test")) {
                Ok(_) => assert!(false),
                Err(_) => assert!(true)
            }
        } 
        
        #[test]
        fn insert_func_decl_correct_use() {
            let mut scope_manager = ScopeManager::new();

            scope_manager.create_new_scope();
            match scope_manager.insert_func_decl(FuncDecl::new(Type::INTEGER, Vec::new()), String::from("test_function")) {
                Ok(_) => assert!(true),
                Err(_) => assert!(false)
            }
        } 

        #[test]
        fn insert_func_decl_incorrect_use() {
            let mut scope_manager = ScopeManager::new();

            match scope_manager.insert_func_decl(FuncDecl::new(Type::INTEGER, Vec::new()), String::from("test_function")) {
                Ok(_) => assert!(false),
                Err(_) => assert!(true)
            }
        } 

    }
}

mod parser {

    use super::lexer;
    use std::collections;
   
    struct AstNode {
        token: lexer::Token,
        left: Option<Box<AstNode>>,
        right: Option<Box<AstNode>>
    }
   


    struct Parser<'a>{
        lexer: lexer::Lexer<'a>,
    }

    impl<'a> Parser<'a> {
        pub fn new(code: &str) -> Parser {
           Parser {
               lexer: lexer::Lexer::new(code),
           }  
        }

        pub fn build_ast(&mut self) -> AstNode {
            let mut root = AstNode {
                token: lexer::Token::new(lexer::TokenType::ROOT, String::from("ROOT")),
                left: None,
                right: None
            }; 
            self.parse(&mut root);
            root
        }

        fn parse(&mut self, root: &mut AstNode) -> Result<(), String> {
           
            let mut current_node = root;
            let lookahead = self.lexer.peek_token();  
            match lookahead.token_type {
                lexer::TokenType::KEYWORD => {
                    match lookahead.text.as_str() {
                        "function" => {
                            let mut lookahead = self.lexer.peek_token();
                            if lookahead.token_type != lexer::TokenType::NAME {
                                return Err(format!("Expected a function name, found \"{}\"", lookahead.text));
                            } 
                            let function_name = lookahead.text;

                            lookahead = self.lexer.peek_token();
                            if lookahead.token_type != lexer::TokenType::LPARENTHESE {
                                return Err(format!("Expected a \"(\", found \"{}\"", lookahead.text));
                            }

                            lookahead = self.lexer.peek_token();
                            while lookahead.token_type != lexer::TokenType::RPARENTHESE {
                                if lookahead.token_type != lexer::TokenType::NAME {
                                    return Err(format!("Expected a function param name, found \"{}\"", lookahead.text));
                                }    
                                lookahead = self.lexer.peek_token();
                                if lookahead.token_type != lexer::TokenType::NAME {
                                    return Err(format!("Expected a function param name, found \"{}\"", lookahead.text));
                                }    
                            } 
                        }
                        _ => {
                            panic!("Crash and Burn. If you see this error, something went very wrong!");
                        }
                    }
                }
                _ => {
                    panic!("Crash and Burn. If you see this error, something went very wrong!");
                }
            }
            Ok(())
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn template() {
            assert_eq!(1 + 1, 2);
        }
    }
}
