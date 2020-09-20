mod lexer {

    struct Lexer<'a> {
        code_iterator: std::iter::Peekable< std::str::Chars<'a> >
    } 

    enum TokenType {
        NAME,
        EOF
    }

    struct Token {
        token_type : TokenType,
        text: String,
    }

    impl Token {
        fn new(token_type: TokenType, text: String) -> Token{
            Token {
                token_type,
                text
            }
        }
    }


    impl <'a> Lexer<'a>{ 
        fn new(raw_code: & 'a str) -> Lexer<'a>{
            Lexer {
                code_iterator: raw_code.chars().peekable()
            }
        }

        fn get_token(&mut self) -> Token {
            
            let c = match self.code_iterator.next() {
                Some(c) => c,
                None => return Token::new(TokenType::EOF, String::from("EOF"))
            };
            
            let result: Token = match c {
                ' ' => consume_whitespace(),
            }

            result
        }
        
        fn comsume(&mut self) -> Result<char, &'static str>{
            let result = match self.code_iterator.next() {
                Some(c) => Ok(c),
                None => return Err("Comsume any empty stream")
            };

            return result
        }

        fn consume_whitespaces(&mut self) {
            let mut lookahead;
            loop { 
                lookahead = match self.code_iterator.peek(){
                    Some(c) => c,
                    None => break
                };
                match &lookahead {
                    ' ' | '\n' | '\t' | '\r' => self.code_iterator.next(),
                } 
            }
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn get_token_text_value() {
            let code = r"
                function one() -> int {
                    return 1;
                }
                ";
            let mut lex: Lexer = Lexer::new(& code);
            assert_eq!(lex.get_token().text, "function");
            assert_eq!(lex.get_token().text, "one");
            assert_eq!(lex.get_token().text, "(");
            assert_eq!(lex.get_token().text, ")");
            assert_eq!(lex.get_token().text, "->");
            assert_eq!(lex.get_token().text, "int");
            assert_eq!(lex.get_token().text, "{");
            assert_eq!(lex.get_token().text, "return");
            assert_eq!(lex.get_token().text, "1");
            assert_eq!(lex.get_token().text, ";");
            assert_eq!(lex.get_token().text, "}");
            assert_eq!(lex.get_token().text, "EOF");
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
