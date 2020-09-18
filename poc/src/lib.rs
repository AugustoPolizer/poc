mod lexer {

    struct Lexer {
        code: String 
    } 

    enum TokenType {
        NAME
    }

    struct Token {
        token_type : TokenType,
        value: String,
    }

    impl Lexer { 
        fn get_token(&self) -> Token {
            Token {
                token_type: TokenType::NAME,
                value: String::from("test"),
            }
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
