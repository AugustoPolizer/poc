mod lexer {

    struct Lexer<'a> {
        code_iterator: std::iter::Peekable<std::str::Chars<'a>>,
        keywords: std::collections::HashSet<&'static str>,
        types: std::collections::HashSet<&'static str>
    }

    #[derive(PartialEq, Debug)]
    enum TokenType {
        NAME,
        KEYWORD,
        TYPE,
        NUMBER,
        STRING,
        ATTR,
        OP,
        ARROW,
        UNARYOP,
        LPARENTHESES,
        RPARENTHESES,
        LBRACK,
        RBRACK,
        LBRACE,
        RBRACE,
        SEMICOLON,
        COLON,
        EOF,
        UNKNOWN
    }


    struct Token {
        token_type: TokenType,
        text: String,
    }

    impl Token {
        fn new(token_type: TokenType, text: String) -> Token {
            Token { token_type, text }
        }
    }

    impl std::cmp::PartialEq for Token {
        fn eq(&self, other: &Self) -> bool {
            self.text == other.text && self.token_type == other.token_type
        }
    }

    impl<'a> Lexer<'a> {
        fn new(raw_code: &'a str) -> Lexer<'a> {
            Lexer {
                code_iterator: raw_code.chars().peekable(),
                keywords: ["function", "return", "if", "else", "let", "const"].iter().cloned().collect(),
                types: ["int", "float", "void", "string"].iter().cloned().collect()
            }
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
            let mut parsing_tokens = | second_possible_char: char, first_token_type: TokenType, second_token_type: TokenType| -> Token {
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

            // TODO: Refator, extrair metodo
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
                        buffer.push(lookahead);
                        self.code_iterator.next();
                        lookahead = match self.code_iterator.peek() {
                            Some(c) => *c,
                            None => break,
                        };
                        if lookahead == '"' {
                            break;
                        }
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
                    Token::new(TokenType::NUMBER, buffer) 
                }
                '<' | '>' =>  {
                    parsing_tokens('=', TokenType::OP, TokenType::OP)
                }
                '=' => {
                    parsing_tokens('=', TokenType::ATTR, TokenType::OP)
                }
                '-' => {
                    parsing_tokens('>', TokenType::OP, TokenType::ARROW)
                }
                '!' => {
                    parsing_tokens('=', TokenType::UNARYOP, TokenType::OP)
                }
                '+' | '*' | '/' => {
                    self.code_iterator.next();
                   Token::new(TokenType::OP, String::from(lookahead))
                }
                ';' => {
                    self.code_iterator.next();
                    Token::new(TokenType::SEMICOLON, String::from(";")) 
                }
                ':' => {
                    self.code_iterator.next();
                    Token::new(TokenType::COLON, String::from(":")) 
                }
                '(' => {
                    self.code_iterator.next();
                    Token::new(TokenType::LPARENTHESES, String::from("(")) 
                }
                ')' => {
                    self.code_iterator.next();
                    Token::new(TokenType::RPARENTHESES, String::from(")")) 
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
            assert_token_equal(&mut lexer, "(", TokenType::LPARENTHESES);
            assert_token_equal(&mut lexer, ")", TokenType::RPARENTHESES);
            assert_token_equal(&mut lexer, "->", TokenType::ARROW);
            assert_token_equal(&mut lexer, "int", TokenType::TYPE);
            assert_token_equal(&mut lexer, "{", TokenType::LBRACE);
            assert_token_equal(&mut lexer, "return", TokenType::KEYWORD);
            assert_token_equal(&mut lexer, "1", TokenType::NUMBER);
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
            assert_token_equal(&mut lexer, "(", TokenType::LPARENTHESES);
            assert_token_equal(&mut lexer, ")", TokenType::RPARENTHESES);
            assert_token_equal(&mut lexer, "->", TokenType::ARROW);
            assert_token_equal(&mut lexer, "void", TokenType::TYPE);
            assert_token_equal(&mut lexer, "{", TokenType::LBRACE);
            assert_token_equal(&mut lexer, "const", TokenType::KEYWORD);
            assert_token_equal(&mut lexer, "a", TokenType::NAME);
            assert_token_equal(&mut lexer, ":", TokenType::COLON);
            assert_token_equal(&mut lexer, "int", TokenType::TYPE);
            assert_token_equal(&mut lexer, "=", TokenType::ATTR);
            assert_token_equal(&mut lexer, "1", TokenType::NUMBER);
            assert_token_equal(&mut lexer, ";", TokenType::SEMICOLON);
            assert_token_equal(&mut lexer, "const", TokenType::KEYWORD);
            assert_token_equal(&mut lexer, "b", TokenType::NAME);
            assert_token_equal(&mut lexer, ":", TokenType::COLON);
            assert_token_equal(&mut lexer, "float", TokenType::TYPE);
            assert_token_equal(&mut lexer, "=", TokenType::ATTR);
            assert_token_equal(&mut lexer, "3.14", TokenType::NUMBER);
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
                function operators() -> float {
                    return (500.01 + 3) * 2.015 + (1 / 3);
                }
                ";
            
            let mut lexer: Lexer = Lexer::new(&code);
           
            assert_token_equal(&mut lexer, "function", TokenType::KEYWORD);
            assert_token_equal(&mut lexer, "operators", TokenType::NAME);
            assert_token_equal(&mut lexer, "(", TokenType::LPARENTHESES);
            assert_token_equal(&mut lexer, ")", TokenType::RPARENTHESES);
            assert_token_equal(&mut lexer, "->", TokenType::ARROW);
            assert_token_equal(&mut lexer, "float", TokenType::TYPE);
            assert_token_equal(&mut lexer, "{", TokenType::LBRACE);
            assert_token_equal(&mut lexer, "return", TokenType::KEYWORD);
            assert_token_equal(&mut lexer, "(", TokenType::LPARENTHESES);
            assert_token_equal(&mut lexer, "500.01", TokenType::NUMBER);
            assert_token_equal(&mut lexer, "+", TokenType::OP);
            assert_token_equal(&mut lexer, "3", TokenType::NUMBER);
            assert_token_equal(&mut lexer, ")", TokenType::RPARENTHESES);
            assert_token_equal(&mut lexer, "*", TokenType::OP);
            assert_token_equal(&mut lexer, "2.015", TokenType::NUMBER);
            assert_token_equal(&mut lexer, "+", TokenType::OP);
            assert_token_equal(&mut lexer, "(", TokenType::LPARENTHESES);
            assert_token_equal(&mut lexer, "1", TokenType::NUMBER);
            assert_token_equal(&mut lexer, "/", TokenType::OP);
            assert_token_equal(&mut lexer, "3", TokenType::NUMBER);
            assert_token_equal(&mut lexer, ")", TokenType::RPARENTHESES);
            assert_token_equal(&mut lexer, ";", TokenType::SEMICOLON);
            assert_token_equal(&mut lexer, "}", TokenType::RBRACE);
            assert_token_equal(&mut lexer, "EOF", TokenType::EOF);
        }

        #[test]
        fn get_token_if_else() {
            let code = r"
                function operators(x: int) -> float {
                    if (x <= 10 && x >= 0) {
                        return 10;
                    } else if (x <= 100) {
                        return 100;
                    } else {
                        return 1000; 
                    }
                }
                ";
            
            let mut lexer: Lexer = Lexer::new(&code);
            assert_token_equal(&mut lexer, "function", TokenType::KEYWORD);
        }
    }
}

mod parser {

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn template() {
            assert_eq!(1 + 1, 2);
        }
    }
}
