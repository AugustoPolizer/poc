# POC-1


# Grammar

- PROG: (STATEMENT)* 
- STATEMENT: FUNCTION | VARIABLE | CMD 
- CMD: IF \ ELSE \ FUNCCALL \ RETURN \ ATTRIBUITION
- IF: 'if' '(' EXPRESSION COMPOPBI EXPRESSION ')' '{' (STATEMENT)* '}' 
- ELSE: 'else' '{' (STATEMENT)* '}' 
- FUNCCALL: NAME '(' FUNCPARAM* ')' ';' 
- RETURN: 'return' EXPRESSION ';'
- ATTRIBUITION: NAME '=' EXPRESSION ';'
- EXPRESSION: EXPRESSION BINARYOP EXPRESSION | '(' EXPRESSION ')' | VALUE
- VALUE: (UNARYOP)? (LITERAL | NAME)
- BINARYOP: '+' | '-' | '*' \ '/' | BINARYOP
- LITERAL: INT | FLOAT | STRING
- FUNCTION: NAME '(' FUNPARAM? ') '{' STATEMENT*'}'
- FUNCPARAM : (NAME ':' NAME) (',' FUNCPARAM)*
- VARIABLE: ('const' | 'let') NAME ':' ( SCALAR | ARRAY) ';'
- SCALAR: NAME ('=' INT | FLOAT | STRING)?
- ARRAY: '[' NAME ']' ('=' '[' (ELEMENTS)']')?
- ELEMENTS: VALUE (',' VALUE)*
- NAME: ALPA (ALPA)*
- UNARYOP: '-' \ '!'
- BOOLBIOP: '&&'|'||'
- COMPOP: '<'|'>'|'<='|'>='|'=='|'!='
- NUMBER: '0'..'9'
- ALPA: ('a'..'z'|'A'..'Z')
- STRING: '"'ALPA*'"'
- INT: NUMBER+
- FLOAT: NUMBER* '.' NUMBER+
