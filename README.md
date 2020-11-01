# POC-1

O objetivo deste trabalho de conclusão de curso é criar uma linguagem de domínio específico, baseada em operações funcionais, para gerar programas eBPF. Além disso, um requisito de projeto é que somente seja possível escrever programas cuja terminação seja garantida. A importância deste trabalho é criar uma linguaguem específica para a tarefa de escrever programas eBPF (extend Berkeley Packet Filter) que seja concisa e que garanta que o programa seja finalizado.

# Keywords
if, else, return, function, let e const

# Binary Operators

+, -, *, /,  &&, ||, <, <=, >, >=

# Grammar
## Programa
- PROG: (STATEMENT)* 
## Comandos
- STATEMENT: FUNCTION | VARIABLE | CMD 
- CMD: IF | ELSE | FUNCCALL | RETURN  | ATTRIBUITION
- IF: 'if' '(' EXPRESSION ')' '{' (STATEMENT)* '}' 
- ELSE: 'else' '{' (STATEMENT)* '}' 
- RETURN: 'return' EXPRESSION ';'
- FUNCCALL: NAME '(' PARAMS? ')' ';'
- VARIABLE: ('const' | 'let') NAME ':' ( SCALAR | ARRAY) ';'
- ATTRIBUITION: NAME '=' EXPRESSION ';'
## Funções
- FUNCTION: 'function' NAME '(' FUNPARAM? ') -> NAME '{' STATEMENT*'}'
- FUNCPARAMS : NAME ':' NAME (',' NAME ':' NAME) 
- PARAMS: NAME (',' NAME)*
## Expressões
- EXPRESSION: EQUALITY
- EQUALITY: COMPARISON ( ("!=" | "==" ) COMPARISON)*
- COMPARISON: TERM ( ( ">=" | "<=" | "<" | ">" ) TERM)*
- TERM: FACTOR ( ("-" | "+") FACTOR)*
- FACTOR: UNARY ( ( "/" | "*") UNARY)*
- UNARY: (( "!" | "-") UNARY) | PRIMARY
- PRIMARY: INT | FLOAT | STRING | 'false' | 'true'; 
## Arrays
- ARRAY: '[' NAME ']' ('=' '[' ELEMENTS ']')?
- ELEMENTS: (VALUE | LITERAL) (',' (VALUE | LITERAL))*
## Outras regras
- SCALAR: 'NAME' ('=' EXPRESSION)?
- NAME: (ALPA | '_') (ALPA | NUMBER | '_')*
- UNARYOP: '-' | '!'
- NUMBER: '0'..'9'
- ALPA: ('a'..'z'|'A'..'Z')
- STRING: '"'.*'"'
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

