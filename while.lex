structure Tokens = Tokens
    type pos = int
    type svalue = Tokens.svalue
    type ('a,'b) token = ('a,'b) Tokens.token  
    type lexresult = (svalue, pos) token

    val pos = ref 0
    val col = ref 1
    val eof = fn () =>(pos := 1; col := 1; Tokens.EOF(!pos, !col))
    val error = fn (e, line:int, col:int) => TextIO.output(TextIO.stdOut,"Illegal Token found at L,C :"^Int.toString(line)^":"^Int.toString(col)^","^e)

    fun init(x, i:int) = (x := i; !x)
    fun increment(a, i:int) = (a := !a + i; !a)
    fun toInt(s:string) = valOf(Int.fromString(s)); 
%%
%header (functor WhileLexFun(structure Tokens:While_TOKENS));

whiteSpace = [\ \t];
alpha = [a-zA-z];
alphanum = [A-Za-z0-9];
digit = [0-9];
others = . ;

%% 

\n => (increment(pos,1); increment(pos,1); init(col,1); lex());
{whiteSpace}+ => (increment(col, size(yytext)); lex());
"program" => (increment(col, size(yytext));Tokens.PROGRAM(!pos, !col)); 
"::" => (increment(col, size(yytext));Tokens.COLCOL(!pos, !col));      
"var" => (increment(col,size(yytext)); Tokens.VAR(!pos, !col));
":" => (increment(col,1); Tokens.COLON(!pos, !col));
"," => (increment(col,1); Tokens.COMMA(!pos, !col) );
"int" => (increment(col,size(yytext)); Tokens.INT(!pos, !col));
"bool" => (increment(col,size(yytext)); Tokens.BOOL(!pos, !col));
"{" => (increment(col,1); Tokens.LBRACE(!pos, !col));
"}" => (increment(col,1); Tokens.RBRACE(!pos, !col));
":=" => (increment(col,1); Tokens.SET(!pos, !col));
";" => ( increment(col,1);Tokens.SEMICOLON(!pos, !col));
"read" => (increment(col,size(yytext)); Tokens.READ(!pos, !col));
"write" => (increment(col,size(yytext)); Tokens.WRITE(!pos, !col));
"if" => (increment(col,size(yytext)); Tokens.IF(!pos, !col) );
"then" => (increment(col,size(yytext)); Tokens.THEN(!pos, !col));
"else" => (increment(col,size(yytext)); Tokens.ELSE(!pos, !col));
"endif" => (increment(col,size(yytext));Tokens.FI(!pos, !col));
"while" => (increment(col,size(yytext)); Tokens.WHILE(!pos, !col));
"do" => (increment(col,size(yytext)); Tokens.DO(!pos, !col) );
"endwh" => (increment(col,size(yytext));Tokens.ELIHW(!pos, !col));
"||" => (increment(col, 2); Tokens.OR(!pos, !col));
"&&"=> (increment(col, 2); Tokens.AND(!pos, !col));
"!" => (increment(col, 1); Tokens.NOT(!pos, !col));
"<"  => (increment(col, size(yytext));Tokens.LT(!pos, !col)); 
"<=" => (increment(col, size(yytext));Tokens.LEQ(!pos, !col));
"="  => (increment(col, size(yytext));Tokens.EQ(!pos, !col));
">"  => (increment(col, size(yytext));Tokens.GT(!pos, !col)); 
">=" => (increment(col, size(yytext));Tokens.GEQ(!pos, !col));
"<>" => (increment(col, size(yytext));Tokens.NEQ(!pos, !col));
"+" => (increment(col, 1);Tokens.PLUS(!pos, !col));
"-" => (increment(col, 1);Tokens.MINUS(!pos, !col));
"*" => (increment(col,1);Tokens.TIMES(!pos, !col));
"/" => (increment(col,1);Tokens.DIV(!pos, !col));
"%" => (increment(col,1);Tokens.MOD(!pos, !col));
"~" => (increment(col,1);Tokens.UNARY(!pos, !col));
"(" => (increment(col,1);Tokens.LPAREN(!pos, !col));
")" => (increment(col,1);Tokens.RPAREN(!pos, !col));
{digit}+ => (increment(col,size(yytext)); Tokens.INTEGER(toInt(yytext), !pos, !col));
"tt" => (increment(col , 2); Tokens.TT(!pos, !col));     
"ff" => (increment(col , 2); Tokens.FF(!pos, !col));
{alpha}{alphanum}* => (increment(col,size(yytext)); Tokens.ID(yytext, !pos, !col));
{others} => (error(yytext,!pos,!col); increment(col, size(yytext)); lex());
