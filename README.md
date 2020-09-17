# POC-1


# Grammar

- PROG : (STATEMENT)* 
- STATEMENT: FUNCTION | VARIABLE | CMD 
- CMD: IF \ ELSE | FUNCCALL | RETURN | ATTRIBUITION
- IF: 'if' '(' EXPRESSION COMPOPBI EXPRESSION ')' '{' (STATEMENT)* '}' 
- ELSE: 'else' '{' (STATEMENT)* '}' 
- FUNCCALL: NAME '(' FUNCPARAM* ')' ';' 
- RETURN: 'return' EXPRESSION ';'
- ATTRIBUITION: NAME '=' NAME | EXPRESSION ';'
- VALUE: 
- LITERAL: INT | FLOAT | STRING
- FUNCTION: NAME '(' FUNPARAM ') '{' STATEMENT*'}'
- FUNCPARAM : (NAME ':' NAME) (',' FUNCPARAM)*
- VARIABLE: ('const' | 'let') NAME : NAME (= INT | FLOAT | STRING)? ';'
- NAME: ALPA (ALPA)*
- BOOLBIOP: '&&'|'||'
- COMPOP: '<'|'>'|'<='|'>='|'=='|'!='
- NUMBER: '0'..'9'
- ALPA: ('a'..'z'|'A'..'Z')
- STRING: '"'ALPA*'"'
- INT: NUMBER+
- FLOAT: NUMBER* '.' NUMBER+
