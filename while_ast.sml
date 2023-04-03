structure AST =
struct
    type ID = string;
    type VariableList = ID list; 
    datatype Type = INT | BOOL
    and BLK = BLK of DECSEQ * SEQ
    and DECSEQ = DECSEQ of DEC list
    and DEC = DEC of Type * VariableList
    and SEQ = SEQ of CMD list
    and AST = PROG of ID * BLK 
    and CMD = ITE of Exp * SEQ * SEQ | WH of Exp * SEQ | SET of ID * Exp | WRITE of Exp | READ of ID 
    and Exp = id of string | INTCONST of int | TT | FF | UNARY of Exp | NOT of Exp | AND of  Exp*Exp | 
            OR of  Exp*Exp | LT of  Exp*Exp | LEQ of Exp*Exp | EQ of Exp*Exp | GT of Exp*Exp | GEQ of Exp*Exp | 
            NEQ of Exp*Exp | PLUS of Exp*Exp | MINUS of Exp*Exp | TIMES of Exp*Exp | DIV of Exp*Exp | MOD of Exp*Exp ;
end