const TAB_WIDTH: u32 = 8;

pub struct Lexer<'a> {
    code_iterator: std::iter::Peekable<std::str::Chars<'a>>,
    keywords: std::collections::HashSet<&'static str>,
    types: std::collections::HashSet<&'static str>,
    peeked_tokens: std::collections::VecDeque<Token>,
    current_line: u32,
    current_column: u32
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
    pub line: u32,
    pub column: u32
}

impl Token {
    pub fn new(token_type: TokenType, text: String, line: u32, column: u32) -> Token {
        Token { token_type, text , line, column}
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
            types: ["int", "float", "string"].iter().cloned().collect(),
            peeked_tokens: std::collections::VecDeque::new(),
            current_line: 1,
            current_column: 1
        }
    }

    fn consume_token(&mut self) -> Token {
        let token = match self.peeked_tokens.pop_front() {
            Some(t) => t,
            None => self.get_token()
        };
        token
    }

    fn peek_token(&mut self) -> Token {
        let token = self.get_token();
        self.peeked_tokens.push_back(token.clone());

        token
    }

    pub fn match_token(&mut self, token_type: TokenType, token_text_options: Vec<&str>) -> (bool, Token) {
        match self.peeked_tokens.front() {
            Some(token) => {
                let token_clone = token.clone();
                if token_clone.token_type == token_type {
                    if token_text_options.is_empty() {
                        self.consume_token();
                        return (true, token_clone);
                    }
                    for text in token_text_options {
                        if text == token_clone.text {
                            self.consume_token();
                            return (true, token_clone);
                        }
                    }
                    return (false, Token::new(TokenType::UNKNOWN, String::from(""),0,0));
                } else {
                    return (false, Token::new(TokenType::UNKNOWN, String::from(""), 0, 0));
                }
            },
            None => {
                let token = self.peek_token();
                if token.token_type == token_type {
                    let token_clone = token.clone();
                    if token_text_options.is_empty() {
                        self.consume_token();
                        return (true, token_clone); 
                    }
                    for text in token_text_options {
                        if text == token_clone.text {
                            self.consume_token();
                            return (true, token_clone);
                        }
                    }
                    return (false, Token::new(TokenType::EOF, String::from(""),0,0));
                } else {
                    return (false, Token::new(TokenType::EOF, String::from(""),0,0));
                }
            }
        };

    }

    fn get_token(&mut self) -> Token {

        let mut lookahead = match self.code_iterator.peek() {
            Some(c) => *c,
            None => return Token::new(TokenType::EOF, String::from("EOF"), 0, 0)
        };

        // Remove whitespaces 
        if lookahead == ' ' || lookahead == '\n' || lookahead == '\t' || lookahead == '\r' {
            self.code_iterator.next();
            self.update_iterator_position_by_witespaces(&lookahead);
            loop {
                lookahead = match self.code_iterator.peek() {
                    Some(c) => *c,
                    None => return Token::new(TokenType::EOF, String::from("EOF"), 0, 0),
                };
                match lookahead {
                    ' ' | '\n' | '\t' | '\r' => { 
                        self.code_iterator.next(); 
                        self.update_iterator_position_by_witespaces(&lookahead);
                    },
                    _ => break,
                };
            }
        }
        
        // Parsing tokens that need a lookahead equal to 2
        let mut parsing_tokens_ll2 = | second_possible_char: char, first_token_type: TokenType, second_token_type: TokenType| -> Token {
            let mut buffer = String::new();
            let start_column = self.current_column;
            buffer.push(lookahead);
            self.consume_char();
            let lookahead_2 = match self.code_iterator.peek() {
                Some(c) => *c,
                None => return Token::new(first_token_type, buffer, self.current_line, start_column)
            };
            if lookahead_2 != second_possible_char {
                return Token::new(first_token_type, buffer, self.current_line, start_column);
            }
            buffer.push(lookahead_2);
            self.consume_char();
            Token::new(second_token_type, buffer, self.current_line, start_column)
        };

        let result = match lookahead {
            'a'..='z' | 'A'..='Z' => {
                let mut buffer = String::new();
                let start_column = self.current_column;
                loop {
                    buffer.push(lookahead);
                    self.consume_char();
                    lookahead = match self.code_iterator.peek() {
                        Some(c) => *c,
                        None => break,
                    };
                    if !lookahead.is_ascii_alphanumeric() && !(lookahead == '_') {
                        break;
                    }
                }
                if self.keywords.contains(buffer.as_str()) {
                    println!("{}, {}", self.current_line, start_column);
                    return Token::new(TokenType::KEYWORD, buffer, self.current_line, start_column);
                } else if self.types.contains(buffer.as_str()){
                    return Token::new(TokenType::TYPE, buffer, self.current_line, start_column);
                }
                Token::new(TokenType::NAME, buffer, self.current_line, start_column)
            }
            '"' => {
                let mut buffer = String::new();
                let start_column = self.current_column;
                loop {
                    self.consume_char();
                    lookahead = match self.code_iterator.peek() {
                        Some(c) => *c,
                        None => break,
                    };
                    if lookahead == '"' {
                        self.consume_char();
                        break;
                    }
                    buffer.push(lookahead);
                }
                Token::new(TokenType::STRING, buffer, self.current_line, start_column)
            }
            '0'..='9' => {
                let mut float = false;
                let mut buffer = String::new();
                let start_column = self.current_column;
                loop {
                    buffer.push(lookahead);
                    self.consume_char();
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
                    return Token::new(TokenType::FLOAT, buffer, self.current_line, start_column) 
                }
                Token::new(TokenType::INTEGER, buffer, self.current_line, start_column) 
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
                let token = Token::new(TokenType::OP, String::from(lookahead), self.current_line, self.current_column);
                self.consume_char();
                token
            }
            ';' => {
                let token = Token::new(TokenType::SEMICOLON, String::from(";"), self.current_line, self.current_column);
                self.consume_char();
                token
            }
            ',' => {
                let token = Token::new(TokenType::COMMA, String::from(","), self.current_line, self.current_column); 
                self.consume_char();
                token
            }
            ':' => {
                let token = Token::new(TokenType::COLON, String::from(":"), self.current_line, self.current_column); 
                self.consume_char();
                token
            }
            '(' => {
                let token = Token::new(TokenType::LPARENTHESE, String::from("("), self.current_line, self.current_column);
                self.consume_char();
                token
            }
            ')' => {
                let token = Token::new(TokenType::RPARENTHESE, String::from(")"), self.current_line, self.current_column);
                self.consume_char();
                token
            }
            '[' => {
                let token = Token::new(TokenType::LBRACK, String::from("["), self.current_line, self.current_column);
                self.consume_char();
                token
            }
            ']' => {
                let token = Token::new(TokenType::RBRACK, String::from("]"), self.current_line, self.current_column);
                self.consume_char();
                token
            }
            '{' => {
                let token = Token::new(TokenType::LBRACE, String::from("{"), self.current_line, self.current_column);
                self.consume_char();
                token
            }
            '}' => {
                let token = Token::new(TokenType::RBRACE, String::from("}"), self.current_line, self.current_column);
                self.consume_char();
                token
            }
            _ => {
                let token = Token::new(TokenType::UNKNOWN, String::from("UNK"), self.current_line, self.current_column);
                self.consume_char();
                token
            }
        };

        result
    }

    fn consume_char(&mut self) {
        self.code_iterator.next();
        self.current_column += 1;
    }

    fn update_iterator_position_by_witespaces(&mut self, whitespace: &char) {
        if *whitespace == ' ' {
            self.current_column += 1;
        } else if *whitespace == '\t' {
            self.current_column += TAB_WIDTH;
        } else if *whitespace == '\n' {
            self.current_line += 1;
            self.current_column = 1;
        } else if *whitespace == '\r' {
            self.current_column = 1;
        }

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

    fn assert_token_equal_consume(lexer: &mut Lexer, text: &str, token_type: TokenType){
        let token = lexer.consume_token();
        assert_eq!(token.text, text);
        assert_eq!(token.token_type, token_type);
    }

    fn assert_token_equal_peek(lexer: &mut Lexer, text: &str, token_type: TokenType){
        let token = lexer.peek_token();
        assert_eq!(token.text, text);
        assert_eq!(token.token_type, token_type);
    }

    fn assert_token_equal_match(lexer: &mut Lexer, options: Vec<&str>, token_type: TokenType, expected_result: (bool, &str)){
        let match_result = lexer.match_token(token_type, options);
        assert_eq!(match_result.0, expected_result.0);
        assert_eq!(match_result.1.text, expected_result.1);
    }

    fn assert_token_equal_current_position(lexer: &mut Lexer, line: u32, column: u32){
        let token = lexer.peek_token();
        assert_eq!(token.line, line);
        assert_eq!(token.column, column);
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
        function attributionTest() {
            const a : int = 1;
            const b : float = 3.14;
            let c : float = a + b;
        }";
        let mut lexer: Lexer = Lexer::new(&code);
        
        assert_token_equal(&mut lexer, "function", TokenType::KEYWORD);
        assert_token_equal(&mut lexer, "attributionTest", TokenType::NAME);
        assert_token_equal(&mut lexer, "(", TokenType::LPARENTHESE);
        assert_token_equal(&mut lexer, ")", TokenType::RPARENTHESE);
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

    #[test]
    fn match_token() {
        let code = r"
            let a: int = 10; 
        ";
        let mut lexer: Lexer = Lexer::new(&code);
        assert_token_equal_match(&mut lexer, vec!["let"], TokenType::KEYWORD, (true, "let"));    
        assert_token_equal_match(&mut lexer, vec!["let", "const", "int"], TokenType::KEYWORD, (false, ""));    
        assert_token_equal_match(&mut lexer, vec!["test"], TokenType::NAME, (false, ""));    
        assert_token_equal_match(&mut lexer, vec![], TokenType::NAME, (true, "a"));    
        assert_token_equal_match(&mut lexer, vec![";", ":"], TokenType::COLON, (true, ":"));    
        assert_token_equal_match(&mut lexer, vec!["int", "float"], TokenType::TYPE, (true, "int"));    
        assert_token_equal_match(&mut lexer, vec!["="], TokenType::ATTR, (true, "="));    
        assert_token_equal_match(&mut lexer, vec!["10"], TokenType::INTEGER, (true, "10"));    
        assert_token_equal_match(&mut lexer, vec![";"], TokenType::SEMICOLON, (true, ";"));    
        assert_token_equal_match(&mut lexer, vec!["EOF"], TokenType::EOF, (true, "EOF"));    
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

    #[test]
    fn test_current_position_with_one_line() {
        let code = r"let a: int = 10;";
        let mut lexer: Lexer = Lexer::new(&code);

        assert_token_equal_current_position(&mut lexer, 1, 1);
        assert_token_equal_current_position(&mut lexer, 1, 5);
        assert_token_equal_current_position(&mut lexer, 1, 6);
        assert_token_equal_current_position(&mut lexer, 1, 8);
        assert_token_equal_current_position(&mut lexer, 1, 12);
        assert_token_equal_current_position(&mut lexer, 1, 14);
        assert_token_equal_current_position(&mut lexer, 1, 16);
        assert_token_equal_current_position(&mut lexer, 0, 0);
        assert_token_equal_current_position(&mut lexer, 0, 0);
    }

    #[test]
    fn test_current_position_with_multiple_lines() {

        let code = r"
function attributionTest() {
    const a : int = 1;
    const b : float = 3.14;
    let c : float = a + b;
}";
        let mut lexer: Lexer = Lexer::new(&code);

        assert_token_equal_current_position(&mut lexer, 2, 1);
        assert_token_equal_current_position(&mut lexer, 2, 10);
        assert_token_equal_current_position(&mut lexer, 2, 25);
        assert_token_equal_current_position(&mut lexer, 2, 26);
        assert_token_equal_current_position(&mut lexer, 2, 28);
        assert_token_equal_current_position(&mut lexer, 3, 5);
        assert_token_equal_current_position(&mut lexer, 3, 11);
        assert_token_equal_current_position(&mut lexer, 3, 13);
        assert_token_equal_current_position(&mut lexer, 3, 15);
        assert_token_equal_current_position(&mut lexer, 3, 19);
        assert_token_equal_current_position(&mut lexer, 3, 21);
        assert_token_equal_current_position(&mut lexer, 3, 22);
        assert_token_equal_current_position(&mut lexer, 4, 5);
        assert_token_equal_current_position(&mut lexer, 4, 11);
        assert_token_equal_current_position(&mut lexer, 4, 13);
        assert_token_equal_current_position(&mut lexer, 4, 15);
        assert_token_equal_current_position(&mut lexer, 4, 21);
        assert_token_equal_current_position(&mut lexer, 4, 23);
        assert_token_equal_current_position(&mut lexer, 4, 27);
        assert_token_equal_current_position(&mut lexer, 5, 5);
        assert_token_equal_current_position(&mut lexer, 5, 9);
        assert_token_equal_current_position(&mut lexer, 5, 11);
        assert_token_equal_current_position(&mut lexer, 5, 13);
        assert_token_equal_current_position(&mut lexer, 5, 19);
        assert_token_equal_current_position(&mut lexer, 5, 21);
        assert_token_equal_current_position(&mut lexer, 5, 23);
        assert_token_equal_current_position(&mut lexer, 5, 25);
        assert_token_equal_current_position(&mut lexer, 5, 26);
        assert_token_equal_current_position(&mut lexer, 6, 1);
        assert_token_equal_current_position(&mut lexer, 0, 0);
    }
}



