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
            // Parsing tokens that need a lookahead equal to 2
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
                        if !lookahead.is_ascii_alphanumeric() && !(lookahead == '_') {
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

    pub struct Symbol {
        symbol_type: Type,
        is_const: bool
    }

    impl Symbol {
        pub fn new(symbol_type: Type, is_const: bool) -> Symbol {
            Symbol{
                symbol_type,
                is_const
            }
        }
    }

    pub struct FuncDecl {
        return_type: Type,
        params: Vec<Symbol>
    }

    impl FuncDecl {
        pub fn new(return_type: Type, params: Vec<Symbol>) -> FuncDecl {
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
    
    pub struct ScopeManager {
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
        
        pub fn find_func_decl(&mut self, name: &str) -> Option<&FuncDecl> {
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
        fn remove_scope() {
            let mut scope_manager = ScopeManager::new();

            scope_manager.create_new_scope();
            match scope_manager.remove_scope() {
                Some(_) => {
                    match scope_manager.remove_scope() {
                        Some(_) => assert!(false),
                        None => assert!(true)
                    }
                },
                None => assert!(false)
            };
        }

        #[test]
        fn insert_symbol_correct_use() {
            let mut scope_manager = ScopeManager::new();

            scope_manager.create_new_scope();
            match scope_manager.insert_symbol(Symbol::new(Type::INTEGER, false), String::from("var_test")) {
                Ok(_) => assert!(true),
                Err(_) => assert!(false)
            }
        } 

        #[test]
        fn insert_symbol_incorrect_use() {
            let mut scope_manager = ScopeManager::new();

            match scope_manager.insert_symbol(Symbol::new(Type::INTEGER, false), String::from("var_test")) {
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

        #[test]
        fn find_symbol_with_only_one_scope() {
            let mut scope_manager = ScopeManager::new();

            scope_manager.create_new_scope();
            match scope_manager.insert_symbol(Symbol::new(Type::INTEGER, true), String::from("var_test")) {
                Ok(_) => assert!(true),
                Err(_) => assert!(false)
            }
            let symbol = match scope_manager.find_symbol("var_test") {
                Some(x) => x,
                None => return assert!(false)
            };
            assert_eq!(symbol.symbol_type, Type::INTEGER); 
            assert_eq!(symbol.is_const, true); 
        } 

        #[test]
        fn find_symbol_with_two_scopes() {
            let mut scope_manager = ScopeManager::new();

            scope_manager.create_new_scope();
            match scope_manager.insert_symbol(Symbol::new(Type::INTEGER, true), String::from("var_test1")) {
                Ok(_) => assert!(true),
                Err(_) => assert!(false)
            }
 
            scope_manager.create_new_scope();
            match scope_manager.insert_symbol(Symbol::new(Type::FLOAT, false), String::from("var_test2")) {
                Ok(_) => assert!(true),
                Err(_) => assert!(false)
            }
            
            let mut symbol = match scope_manager.find_symbol("var_test2") {
                Some(x) => x,
                None => return assert!(false)
            };
            assert_eq!(symbol.symbol_type, Type::FLOAT); 
            assert_eq!(symbol.is_const, false); 
            
            scope_manager.remove_scope();
            match scope_manager.find_symbol("var_test2") {
                Some(_) => assert!(false),
                None => assert!(true)
            };

            symbol = match scope_manager.find_symbol("var_test1") {
                Some(x) => x,
                None => return assert!(false)
            };
            assert_eq!(symbol.symbol_type, Type::INTEGER); 
            assert_eq!(symbol.is_const, true); 

            scope_manager.remove_scope();
            match scope_manager.find_symbol("var_test1") {
                Some(_) => assert!(false),
                None => assert!(true)
            };

        } 
        
        #[test]
        fn find_func_decl_with_only_one_scope() {
            let mut scope_manager = ScopeManager::new();

            scope_manager.create_new_scope();
            match scope_manager.insert_func_decl(FuncDecl::new(Type::INTEGER, Vec::new()), String::from("test_function")) {
                Ok(_) => assert!(true),
                Err(_) => assert!(false)
            }
            let func_decl = match scope_manager.find_func_decl("test_function") {
                Some(x) => x,
                None => return assert!(false)
            };
            assert_eq!(func_decl.return_type, Type::INTEGER); 
        } 

        #[test]
        fn find_func_decl_with_two_scopes() {
            let mut scope_manager = ScopeManager::new();

            scope_manager.create_new_scope();
            match scope_manager.insert_func_decl(FuncDecl::new(Type::INTEGER, Vec::new()), String::from("test_function1")) {
                Ok(_) => assert!(true),
                Err(_) => assert!(false)
            }
 
            scope_manager.create_new_scope();
            match scope_manager.insert_func_decl(FuncDecl::new(Type::FLOAT, Vec::new()), String::from("test_function2")) {
                Ok(_) => assert!(true),
                Err(_) => assert!(false)
            }
            
            let mut func_decl = match scope_manager.find_func_decl("test_function2") {
                Some(x) => x,
                None => return assert!(false)
            };
            assert_eq!(func_decl.return_type, Type::FLOAT); 
            
            scope_manager.remove_scope();
            match scope_manager.find_func_decl("test_function2") {
                Some(_) => assert!(false),
                None => assert!(true)
            };

            func_decl = match scope_manager.find_func_decl("test_function1") {
                Some(x) => x,
                None => return assert!(false)
            };
            assert_eq!(func_decl.return_type, Type::INTEGER); 

            scope_manager.remove_scope();
            match scope_manager.find_func_decl("test_function1") {
                Some(_) => assert!(false),
                None => assert!(true)
            };

        } 
    }
}

mod error_msgs {

    pub mod parser {
        pub enum ExpectedToken {
            FUNCNAME,
            VARNAME,
            PARAMNAME,
            TYPE,
            COLON,
            LPARENTHESE,
            COMMAORRPARENTHESE,
        }

        pub fn wrong_token_error_msg_handle(error_type: ExpectedToken, wrong_token: &str) -> String {
            match error_type {
                ExpectedToken::FUNCNAME => format!("Expected a function name, found \"{}\"", wrong_token),
                ExpectedToken::PARAMNAME => format!("Expected a param name, found \"{}\"", wrong_token),
                ExpectedToken::VARNAME => format!("Expected a variable name, found \"{}\"", wrong_token),
                ExpectedToken::TYPE => format!("Expected a type, found \"{}\"", wrong_token),
                ExpectedToken::COLON => format!("Expected a \":\", found \"{}\"", wrong_token),
                ExpectedToken::LPARENTHESE => format!("Expected a \"(\", found \"{}\"", wrong_token),
                ExpectedToken::COMMAORRPARENTHESE => format!("Expected a \",\" or a \")\", found \"{}\"", wrong_token)
            }
        }

        #[cfg(test)]
        mod tests {
            use super::*;

            #[test]
            fn func_name_error_msg() {
                let error_msg = wrong_token_error_msg_handle(ExpectedToken::FUNCNAME, "*");
                assert_eq!(error_msg, "Expected a function name, found \"*\"")
            }

            #[test] 
            fn param_name_error_msg() {
                let error_msg = wrong_token_error_msg_handle(ExpectedToken::PARAMNAME, "+");
                assert_eq!(error_msg, "Expected a param name, found \"+\"")
            }
            
            #[test] 
            fn var_name_error_msg() {
                let error_msg = wrong_token_error_msg_handle(ExpectedToken::VARNAME, ":");
                assert_eq!(error_msg, "Expected a variable name, found \":\"")
            }
            
            #[test]
            fn type_error_msg() {
                let error_msg = wrong_token_error_msg_handle(ExpectedToken::TYPE, "let");
                assert_eq!(error_msg, "Expected a type, found \"let\"")
            }

            #[test]
            fn colon_error_msg() {
                let error_msg = wrong_token_error_msg_handle(ExpectedToken::COLON, "function");
                assert_eq!(error_msg, "Expected a \":\", found \"function\"")
            }

            #[test]
            fn lparenthese_error_msg() {
                let error_msg = wrong_token_error_msg_handle(ExpectedToken::LPARENTHESE, "var_name");
                assert_eq!(error_msg, "Expected a \"(\", found \"var_name\"")
            }
            
            #[test]
            fn comma_or_rparenthese_error_msg() {
                let error_msg = wrong_token_error_msg_handle(ExpectedToken::COMMAORRPARENTHESE, "var_name");
                assert_eq!(error_msg, "Expected a \",\" or a \")\", found \"var_name\"")
            }

        }
    }
}

mod parser {

    use super::{lexer, scope_manager, error_msgs};
   
    
    #[derive(PartialEq, Clone)]
    pub enum NodeType {
        ATTR,
        EXPR,
        ROOT,
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
           
            let mut current_node = root;
            let mut lookahead = self.lexer.peek_token();  
            match lookahead.token_type {
                lexer::TokenType::KEYWORD => {
                    match lookahead.text.as_str() {
                        "function" => {
                            lookahead = self.lexer.peek_token();
                            if lookahead.token_type != lexer::TokenType::NAME {
                                return Err(error_msgs::parser::wrong_token_error_msg_handle(
                                        error_msgs::parser::ExpectedToken::FUNCNAME, 
                                        &lookahead.text
                                        ));
                            } 
                            let function_name = lookahead.text;

                            lookahead = self.lexer.peek_token();
                            if lookahead.token_type != lexer::TokenType::LPARENTHESE {
                                return Err(error_msgs::parser::wrong_token_error_msg_handle(
                                        error_msgs::parser::ExpectedToken::LPARENTHESE, 
                                        &lookahead.text
                                        ));
                            }

                            lookahead = self.lexer.peek_token();
                            loop {
                                if lookahead.token_type != lexer::TokenType::NAME {
                                    return Err(error_msgs::parser::wrong_token_error_msg_handle(
                                        error_msgs::parser::ExpectedToken::FUNCNAME, 
                                        &lookahead.text
                                        ));
                                }    
                                lookahead = self.lexer.peek_token();
                                
                                if lookahead.token_type != lexer::TokenType::COLON {
                                    return Err(error_msgs::parser::wrong_token_error_msg_handle(
                                        error_msgs::parser::ExpectedToken::COLON, 
                                        &lookahead.text
                                        ));
                                }    
                                
                                lookahead = self.lexer.peek_token();
                                
                                if lookahead.token_type != lexer::TokenType::TYPE {
                                    return Err(error_msgs::parser::wrong_token_error_msg_handle(
                                        error_msgs::parser::ExpectedToken::TYPE, 
                                        &lookahead.text
                                        ));
                                }    

                                match lookahead.token_type {
                                    lexer::TokenType::RPARENTHESE => break,
                                    lexer::TokenType::COMMA => (),
                                    _ => return Err(error_msgs::parser::wrong_token_error_msg_handle(
                                        error_msgs::parser::ExpectedToken::COMMAORRPARENTHESE, 
                                        &lookahead.text
                                        ))
                                };
                            } 
                        }
                        "let" | "const" => {
                            lookahead = self.lexer.peek_token(); 
                            if lookahead.token_type != lexer::TokenType::NAME { 
                                    return Err(error_msgs::parser::wrong_token_error_msg_handle(
                                        error_msgs::parser::ExpectedToken::VARNAME, 
                                        &lookahead.text
                                        ));
                            }
                            let var_name = lookahead.text;
                            
                            lookahead = self.lexer.peek_token();
                            if lookahead.token_type != lexer::TokenType::COLON {
                                    return Err(error_msgs::parser::wrong_token_error_msg_handle(
                                        error_msgs::parser::ExpectedToken::COLON, 
                                        &lookahead.text
                                        ));
                            }

                            lookahead = self.lexer.peek_token();
                            if lookahead.token_type != lexer::TokenType::TYPE {
                                    return Err(error_msgs::parser::wrong_token_error_msg_handle(
                                        error_msgs::parser::ExpectedToken::TYPE, 
                                        &lookahead.text
                                        ));
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
