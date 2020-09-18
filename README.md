# POC-1


# Grammar

- PROG: (STATEMENT)* 
- STATEMENT: FUNCTION | VARIABLE | CMD 
- CMD: IF | ELSE | FUNCCALL | RETURN  | ATTRIBUITION
- IF: 'if' '(' EXPRESSION COMPOPBI EXPRESSION ')' '{' (STATEMENT)* '}' 
- ELSE: 'else' '{' (STATEMENT)* '}' 
- FUNCCALL: NAME '(' PARAMS? ')' ';' 
- FUNCTION: NAME '(' FUNPARAM? ') -> NAME '{' STATEMENT*'}'
- FUNCPARAMS : NAME ':' NAME (',' NAME ':' NAME) 
- PARAMS: NAME (',' NAME)*
- RETURN: 'return' EXPRESSION ';'
- ATTRIBUITION: NAME '=' EXPRESSION ';'
- EXPRESSION: EXPRESSION BINARYOP EXPRESSION | '(' EXPRESSION ')' | VALUE
- VALUE: (UNARYOP)? (LITERAL | NAME)
- BINARYOP: '+' | '-' | '*' | '/' | BOOLBIOP
- LITERAL: INT | FLOAT | STRING
- VARIABLE: ('const' | 'let') NAME ':' ( SCALAR | ARRAY) ';'
- SCALAR: NAME ('=' LITERAL | VALUE)?
- ARRAY: '[' NAME ']' ('=' '[' ELEMENTS ']')?
- ELEMENTS: (VALUE | LITERAL) (',' (VALUE | LITERAL))*
- NAME: ALPA (ALPA)*
- UNARYOP: '-' | '!'
- BOOLBIOP: '&&'|'||'
- COMPOP: '<'|'>'|'<='|'>='|'=='|'!='
- NUMBER: '0'..'9'
- ALPA: ('a'..'z'|'A'..'Z')
- STRING: '"'ALPA*'"'
- INT: NUMBER+
- FLOAT: NUMBER* '.' NUMBER+

# Code Example
```typescript
function fatorial(n : int) -> int {
    if ( n == 1 ) {
        return 1;
    } else {
        return n * fatorial(n - 1);
    }
}
```

