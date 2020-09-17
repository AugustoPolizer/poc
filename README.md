# POC-1


# Grammar

- PROG : (STATEMENT)* 
- STATEMENT: FUNCTION | VARIABLE | CMD 
- CMD: IF \ CALL | RETURN | ATTRIBUITION
- IF: VALUE COMPOPBI VALUE
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
