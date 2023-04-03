(* User  declarations *)
open AST;
fun push_front(x, L) = x::L ;
val SymbolTable = ref [""];
fun NotDeclared(x:string) = print("ERROR : The ID : "^x^" was not declared.\n");
fun DeclaredAgain(x:string) = print("ERROR : The ID : "^x^" already declared before. \n");
fun search(a::A, x) = if (a = x) then true else search(A,x)
    | search([], x) = false;

%%
(* required declarations *)
%name While

%term PROGRAM | ID of string | TT | FF | INTEGER of int |
    LBRACE | RBRACE | 
    COLCOL |VAR | COMMA | COLON | BOOL | INT | SEMICOLON |
    SET | READ | WRITE |
    IF | THEN |ELSE |  FI | WHILE | DO | ELIHW |
    OR | AND | NOT | 
    UNARY| LPAREN | RPAREN |
    LT | LEQ | EQ | GT | GEQ | NEQ |
    PLUS | MINUS |
    TIMES | DIV | MOD |
    EOF | ILLCH

%nonterm Program of AST.AST | Block of AST.BLK | DeclarationSeq of AST.DECSEQ | 
        DeclarationList of AST.DEC list | Declaration of AST.DEC | VarList of AST.VariableList | CommandSeq of AST.SEQ | 
        CommandList of AST.CMD list |Command of AST.CMD | Expression of AST.Exp | Factor of AST.Exp 

(*
    Precedence order that I take is similar to C-language : 
MAX 1. Bracket  
    2. ~
    3. NOT 
    4. MultOP => * , /, %
    5. AddOp => +, - 
    6. Relational Operatoins
    7. And
MIN 8. Or      
*)
%left OR 
%left AND 
%left LT  LEQ  EQ  GT  GEQ  NEQ 
%left PLUS MINUS
%left DIV MOD TIMES 
%left NOT 
%left UNARY

%eop EOF
%noshift EOF
%pos int 
%verbose

%%

Program : PROGRAM ID COLCOL Block (AST.PROG(ID, Block))
Block : DeclarationSeq CommandSeq (AST.BLK(DeclarationSeq, CommandSeq))
DeclarationSeq : DeclarationList (AST.DECSEQ(DeclarationList))
DeclarationList : Declaration DeclarationList (push_front(Declaration, DeclarationList))
                | ([])      
Declaration : VAR VarList COLON BOOL SEMICOLON ( AST.DEC(BOOL, VarList))
            | VAR VarList COLON INT SEMICOLON ( AST.DEC(INT ,VarList))
VarList : ID COMMA VarList ( push_front(ID, VarList))
        | ID ([ID])
        | ([])
CommandSeq : LBRACE CommandList RBRACE (AST.SEQ(CommandList))
CommandList : Command SEMICOLON CommandList (push_front(Command, CommandList))
            | ([])
Command : ID SET Expression  (AST.SET(ID, Expression))
        | IF Expression THEN CommandSeq ELSE CommandSeq FI (AST.ITE(Expression, CommandSeq1, CommandSeq2))
        | WHILE Expression DO CommandSeq ELIHW (AST.WH(Expression1, CommandSeq))
        | READ ID (AST.READ(ID))
        | WRITE Expression (AST.WRITE(Expression))

Expression : Expression OR Expression (AST.OR(Expression1, Expression2))
           | Expression AND Expression (AST.AND(Expression1, Expression2))
           | Expression LT Expression (AST.LT(Expression1, Expression2))
           | Expression LEQ Expression (AST.LEQ(Expression1, Expression2))
            | Expression EQ Expression (AST.EQ(Expression1, Expression2))
            | Expression GT Expression (AST.GT(Expression1, Expression2))
            | Expression GEQ Expression (AST.GEQ(Expression1, Expression2))
            | Expression NEQ Expression (AST.NEQ(Expression1, Expression2))
            | Expression PLUS Expression (AST.PLUS(Expression1, Expression2))
            | Expression MINUS Expression (AST.MINUS(Expression1, Expression2))
            | Expression TIMES Expression (AST.TIMES(Expression1, Expression2))
            | Expression DIV Expression (AST.DIV(Expression1, Expression2))
            | Expression MOD Expression (AST.MOD(Expression1, Expression2))
            | NOT Expression (AST.NOT(Expression))
            | UNARY Expression (AST.UNARY(Expression))
            | LPAREN Expression RPAREN (Expression) 
            | INTEGER(AST.INTCONST(INTEGER)) | TT (AST.TT)| FF(AST.FF) | ID(AST.id(ID)) | PLUS INTEGER  (AST.INTCONST(INTEGER)) 



