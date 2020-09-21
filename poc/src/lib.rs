mod lexer {

    struct Lexer<'a> {
        code_iterator: std::iter::Peekable<std::str::Chars<'a>>,
    }

    enum TokenType {
        NAME,
        LPARENTHESES,
        RPARENTHESES,
        LBRACK,
        RBRACK,
        LBRACE,
        RBRACE,
        SEMICOLON,
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

    impl<'a> Lexer<'a> {
        fn new(raw_code: &'a str) -> Lexer<'a> {
            Lexer {
                code_iterator: raw_code.chars().peekable(),
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
            println!("valor do lookahead: {}", lookahead);

            // FIXME: Não retornar NAME para todas as cadeias que começam com caracteres
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
                        // FIXME: Incompleto, tem que checar mais coisas!!
                        if !lookahead.is_ascii_alphabetic() && !(lookahead == '_') {
                            break
                        }
                    }
                    Token::new(TokenType::NAME, buffer)
                }
                ';' => {
                    self.code_iterator.next();
                    Token::new(TokenType::SEMICOLON, String::from(";")) 
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
        fn get_token_text_value() {
            let code = r"
                function one() -> int {
                    return 1;
                }
                ";
            let mut lexer: Lexer = Lexer::new(&code);

            assert_token_equal(&mut lexer, "function", TokenType::NAME);
            assert_token_equal(&mut lexer, "one", TokenType::NAME);
            assert_token_equal(&mut lexer, "(", TokenType::LPARENTHESES);
            assert_token_equal(&mut lexer, ")", TokenType::RPARENTHESES);
            assert_token_equal(&mut lexer, "UNK", TokenType::UNKNOWN);
            assert_token_equal(&mut lexer, "UNK", TokenType::UNKNOWN);
            assert_token_equal(&mut lexer, "int", TokenType::NAME);
            assert_token_equal(&mut lexer, "{", TokenType::LBRACE);
            assert_token_equal(&mut lexer, "return", TokenType::NAME);
            assert_token_equal(&mut lexer, "UNK", TokenType::UNKNOWN);
            assert_token_equal(&mut lexer, ";", TokenType::SEMICOLON);
            assert_token_equal(&mut lexer, "}", TokenType::RBRACE);
            assert_token_equal(&mut lexer, "EOF", TokenType::EOF);
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
