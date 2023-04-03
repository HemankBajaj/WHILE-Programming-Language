structure WhileLrVals = WhileLrValsFun(structure Token = LrParser.Token)
structure WhileLex = WhileLexFun(structure Tokens = WhileLrVals.Tokens);
structure WhileParser =
	  Join(structure LrParser = LrParser
     	       structure ParserData = WhileLrVals.ParserData
     	       structure Lex = WhileLex)
open AST;
open FunStack; 

exception VarNotDeclared; 
fun invoke(lexstream) =
    	let 
            fun print_error (s,pos:int,_) =  TextIO.output(TextIO.stdOut, "Error, line number : " ^ (Int.toString pos) ^ "," ^ s ^ "\n")
		in
		    WhileParser.parse(0,lexstream,print_error,())
		end

fun stringToLexer (str) =
    let val done = ref false
    	val lexer=  WhileParser.makeLexer (fn _ => if (!done) then "" else (done:=true;str))
    in
	    lexer
    end	

fun parse (lexer) =
    let val dummyEOF = WhileLrVals.Tokens.EOF(0,0)
    	val (result, lexer) = invoke lexer
	val (nextToken, lexer) = WhileParser.Stream.get lexer
    in
        if WhileParser.sameToken(nextToken, dummyEOF) then result
 	    else (TextIO.output(TextIO.stdOut, "Token Rejected \n"); result)
    end

fun generateAST (file:string)= 
    let 
        fun readProgram(filename:string) =
            let 
                val file = TextIO.openIn filename
                val codeFile = TextIO.inputAll file
                val closeFile = TextIO.closeIn file
            in 
                codeFile^""
            end;
        val prog = readProgram(file) ;
        val ast = parse(stringToLexer(prog));
    in 
        ast
    end;

fun getCommandSeq(s : string) = 
    let 
        val x = generateAST s 
        val (PROG(_, BLK(_, l))) = x
    in 
        l 
    end ;

fun getDeclarationList(s: string) = 
    let     
        val x = generateAST s 
        val (PROG(_, BLK(DECSEQ(l), _))) = x
    in 
        l 
    end; 

fun createListUtil(size, l) =
    if size = 0 then l else createListUtil(size-1, ~1::l);
fun createList(size) = 
    createListUtil(size, []);

val maxMemSize = 71;
val memList = createList(maxMemSize);
val M = Array.fromList(memList);

fun findLocation(mem) = 
    let 
        fun findUtil (mem, i) = 
            if (Array.sub(mem, i) = ~1) then i 
            else findUtil(mem, i+1);

    in 
        findUtil(mem, 0)
    end; 

datatype var = VAR of string*string*int ;

fun decToVarList(DEC(a : Type , ls)) = 
    let 
        val t = if a = INT then "int" else "bool"
        fun listToConstList(t, l) = 
            if l = [] then []
            else 
                let 
                    val ind = findLocation(M);
                    val upd = Array.update(M, ind, 0); 
                in VAR(hd(l), t, ind) :: listToConstList(t, tl(l))
                end;
    in listToConstList(t, ls)
    end; 

fun decListToVarList(decSeq) = 
    if length decSeq = 0 then []
    else decToVarList(hd(decSeq)) @ decListToVarList(tl(decSeq));

fun generateSymbolTable(s : string) = decListToVarList(getDeclarationList(s)); 
print ("Please Enter the name of the Program : ");
val filename = valOf(TextIO.inputLine TextIO.stdIn);
val symbolTable = generateSymbolTable(String.substring(filename, 0, size(filename)-1)); 
(* val symbolTable = generateSymbolTable("TEST1.wh"); *)
fun findVariable(idx, symbolTable) = 
    if symbolTable = [] then raise VarNotDeclared
    else 
        let 
            val VAR(name, _ , ind) = hd(symbolTable)
        in 
            if name = idx then ind 
            else findVariable(idx, tl(symbolTable))
        end ;
fun findTypeVar(idx, symbolTable) = 
    if symbolTable = [] then raise VarNotDeclared
    else 
        let 
            val VAR(name, types ,_) = hd(symbolTable)
        in 
            if name = idx then types
            else findTypeVar(idx, tl(symbolTable))
        end ;

datatype CmdStackElement =  CMD of string | ITEstk of CmdStackElement list*CmdStackElement list*CmdStackElement list |
                        WHstk of CmdStackElement list *CmdStackElement list  | INS of string  | SETstk of string * CmdStackElement list|
                        READstk of string; 

fun ExpPostFix (INTCONST c) = [CMD (Int.toString(c))] 
|   ExpPostFix (PLUS(exp1, exp2)) = ExpPostFix(exp1) @ ExpPostFix(exp2) @ [CMD "PLUS"]
|   ExpPostFix (MINUS(exp1, exp2)) = ExpPostFix(exp1) @ ExpPostFix(exp2) @ [CMD "MINUS"]
|   ExpPostFix (DIV(exp1, exp2)) = ExpPostFix(exp1) @ ExpPostFix(exp2) @ [CMD "DIV"]
|   ExpPostFix (TIMES(exp1, exp2)) = ExpPostFix(exp1) @ ExpPostFix(exp2) @ [CMD "TIMES"]
|   ExpPostFix (MOD(exp1, exp2)) = ExpPostFix(exp1) @ ExpPostFix(exp2) @ [CMD "MOD"]
|   ExpPostFix (id(var)) = [CMD var]
|   ExpPostFix (AND(exp1, exp2)) = ExpPostFix(exp1) @ ExpPostFix(exp2) @ [CMD "AND"]
|   ExpPostFix (OR(exp1, exp2)) = ExpPostFix (exp1) @ ExpPostFix(exp2) @ [CMD "OR"]
|   ExpPostFix (TT) = [CMD "1"]
|   ExpPostFix (FF) = [CMD "0"]
|   ExpPostFix (LT(exp1,exp2)) = ExpPostFix(exp1) @ ExpPostFix(exp2) @ [CMD "LT"]
|   ExpPostFix (LEQ(exp1, exp2)) = ExpPostFix(exp1) @ ExpPostFix(exp2) @ [CMD "LEQ"]
|   ExpPostFix (EQ(exp1, exp2)) = ExpPostFix(exp1) @ ExpPostFix(exp2) @ [CMD "EQ"]
|   ExpPostFix (GT(exp1,exp2)) = ExpPostFix(exp1) @ ExpPostFix(exp2) @ [CMD "GT"]
|   ExpPostFix (GEQ(exp1, exp2)) = ExpPostFix(exp1) @ ExpPostFix(exp2) @ [CMD "GEQ"]
|   ExpPostFix (UNARY(exp)) = ExpPostFix(exp) @ [CMD "UNARY"]
|   ExpPostFix (NOT(exp)) = ExpPostFix(exp) @ [CMD "NOT"]; 

fun mapx (func , x::xs) = func(x) @ mapx(func, xs)
|   mapx (_, []) = [];

fun CmdPostFix (SET (var, exp)) = CMD var ::  ExpPostFix(exp) @ [CMD "SET"]
|   CmdPostFix (WH (exp, SEQ(seq))) = [WHstk(ExpPostFix(exp) ,  mapx(CmdPostFix, seq))]
|   CmdPostFix(ITE(exp, SEQ(seq1) , SEQ(seq2))) = [ITEstk(ExpPostFix(exp), mapx(CmdPostFix, seq1) ,mapx(CmdPostFix, seq2))] 
|   CmdPostFix(WRITE(exp)) = ExpPostFix(exp) @[CMD "WRITE"]
|   CmdPostFix(READ(var)) = [CMD var, CMD "READ"];

fun SeqPostFix(SEQ(CmdSeq)) = mapx(CmdPostFix, CmdSeq);

fun MakeCmdStack(s : string) = 
    let 
        val cmdseq =  getCommandSeq(s : string) 
        val CmdPostList = SeqPostFix(cmdseq)
    in
        list2stack(CmdPostList)
    end ;

fun updateVariable(idx,v, mem)  = 
    let 
        val ind = findVariable(idx, symbolTable)
        val upd = Array.update(mem, ind, v)
    in ()
    end ;
fun isIdentifier(s : string) = List.all (Char.isDigit) (explode s) orelse (String.sub(s,0) = #"~" andalso isIdentifier(String.substring(s, 1, size(s)-1)));
fun getPairStr(a: string, b: string) = 
    let 
        val x = if (isIdentifier(a) = true) then a else Int.toString(Array.sub(M, findVariable(a, symbolTable)))
        val y = if (isIdentifier(b) = true) then b else Int.toString(Array.sub(M, findVariable(b, symbolTable)))
    in (x, y)
    end;
exception WrongType;
fun getPairBool(a:string, b:string) = 
    let 
        val x = if (isIdentifier(a) = true) then a else if(isIdentifier(a) = false andalso findTypeVar(a, symbolTable) = "bool") then Int.toString(Array.sub(M, findVariable(a, symbolTable))) else raise WrongType  
        val y = if (isIdentifier(b) = true) then b else if(isIdentifier(b) = false andalso findTypeVar(b, symbolTable) = "bool") then Int.toString(Array.sub(M, findVariable(b, symbolTable))) else raise WrongType  
    in (x, y)
    end;
fun getPairInt(a:string, b:string) = 
    let 
        val x = if (isIdentifier(a) = true) then a else if(isIdentifier(a) = false andalso findTypeVar(a, symbolTable) = "bool") then raise WrongType else Int.toString(Array.sub(M, findVariable(a, symbolTable)))
        val y = if (isIdentifier(b) = true) then b else if(isIdentifier(b) = false andalso findTypeVar(b, symbolTable) = "bool") then raise WrongType else Int.toString(Array.sub(M, findVariable(b, symbolTable)))
    in (x, y)
    end;
fun getVarInt(a:string) = 
    if (isIdentifier(a) = true) then a 
    else if(isIdentifier(a) = false andalso findTypeVar(a, symbolTable) = "bool") then raise WrongType 
    else Int.toString(Array.sub(M, findVariable(a, symbolTable)));
fun getVarBool(a:string) = 
    if (isIdentifier(a) = true) then a 
    else if(isIdentifier(a) = false andalso findTypeVar(a, symbolTable) = "int") then raise WrongType 
    else Int.toString(Array.sub(M, findVariable(a, symbolTable)));
fun getVar(a:string) = if(isIdentifier(a) = true) then a else Int.toString(Array.sub(M, findVariable(a, symbolTable)));
fun plus(a:string, b:string) = 
    Int.toString(valOf(Int.fromString(a)) + valOf(Int.fromString(b)));
fun minus(a:string, b:string) = 
    Int.toString(valOf(Int.fromString(a)) - valOf(Int.fromString(b)));
fun times(a:string, b:string) = 
    Int.toString(valOf(Int.fromString(a)) * valOf(Int.fromString(b)));
fun divide(a:string, b:string) = 
    Int.toString(valOf(Int.fromString(a)) div valOf(Int.fromString(b)));
fun modulo(a:string, b:string) = 
    Int.toString(valOf(Int.fromString(a)) mod valOf(Int.fromString(b)));
fun or(a:string, b:string) = 
    Int.toString(if (valOf(Int.fromString(a)) = 1) then 1 else valOf(Int.fromString(b)));
fun And(a:string, b:string) = 
    Int.toString(if (valOf(Int.fromString(a)) = 1 andalso valOf(Int.fromString(b)) = 1) then 1 else 0);
fun lt(a:string, b:string) = 
    Int.toString(if (valOf(Int.fromString(a)) < valOf(Int.fromString(b)) ) then 1 else 0);
fun leq(a:string, b:string) = 
    Int.toString(if (valOf(Int.fromString(a)) <= valOf(Int.fromString(b)) ) then 1 else 0);
fun eq(a:string, b:string) = 
    Int.toString(if (valOf(Int.fromString(a)) = valOf(Int.fromString(b)) ) then 1 else 0);
fun gt(a:string, b:string) = 
    Int.toString(if (valOf(Int.fromString(a)) > valOf(Int.fromString(b)) ) then 1 else 0);
fun geq(a:string, b:string) = 
    Int.toString(if (valOf(Int.fromString(a)) >= valOf(Int.fromString(b)) ) then 1 else 0);
fun not(a:string) = Int.toString(1-valOf(Int.fromString(a)));
fun unary(a:string) = Int.toString((0-1)*valOf(Int.fromString(a)));
fun f(s:string) = s;
fun ExpPostFix1 (INTCONST c) = [Int.toString(c)] 
|   ExpPostFix1 (PLUS(exp1, exp2)) = ExpPostFix1(exp1) @ ExpPostFix1(exp2) @ ["PLUS"]
|   ExpPostFix1 (MINUS(exp1, exp2)) = ExpPostFix1(exp1) @ ExpPostFix1(exp2) @ ["MINUS"]
|   ExpPostFix1 (DIV(exp1, exp2)) = ExpPostFix1(exp1) @ ExpPostFix1(exp2) @ ["DIV"]
|   ExpPostFix1 (TIMES(exp1, exp2)) = ExpPostFix1(exp1) @ ExpPostFix1(exp2) @ [ "TIMES"]
|   ExpPostFix1 (MOD(exp1, exp2)) = ExpPostFix1(exp1) @ ExpPostFix1(exp2) @ [ "MOD"]
|   ExpPostFix1 (id(var)) = [var]
|   ExpPostFix1 (AND(exp1, exp2)) = ExpPostFix1(exp1) @ ExpPostFix1(exp2) @ [ "AND"]
|   ExpPostFix1 (OR(exp1, exp2)) = ExpPostFix1 (exp1) @ ExpPostFix1(exp2) @ [ "OR"]
|   ExpPostFix1 (TT) = ["TRUE"]
|   ExpPostFix1 (FF) = ["FALSE"]
|   ExpPostFix1 (LT(exp1,exp2)) = ExpPostFix1(exp1) @ ExpPostFix1(exp2) @ [ "LT"]
|   ExpPostFix1 (LEQ(exp1, exp2)) = ExpPostFix1(exp1) @ ExpPostFix1(exp2) @ [ "LEQ"]
|   ExpPostFix1 (EQ(exp1, exp2)) = ExpPostFix1(exp1) @ ExpPostFix1(exp2) @ [ "EQ"]
|   ExpPostFix1 (GT(exp1,exp2)) = ExpPostFix1(exp1) @ ExpPostFix1(exp2) @ [ "GT"]
|   ExpPostFix1 (GEQ(exp1, exp2)) = ExpPostFix1(exp1) @ ExpPostFix1(exp2) @ [ "GEQ"]
|   ExpPostFix1 (UNARY(exp)) = ExpPostFix1(exp) @ [ "UNARY"]
|   ExpPostFix1 (NOT(exp)) = ExpPostFix1(exp) @ [ "NOT"]; 

fun CmdPostFix1 (SET (var, exp)) =  var ::  ExpPostFix1(exp) @ ["SET"]
|   CmdPostFix1 (WH (exp, SEQ(seq))) = ExpPostFix1(exp) @  mapx(CmdPostFix1, seq) @ ["WH"]
|   CmdPostFix1(ITE(exp, SEQ(seq1) , SEQ(seq2))) = ExpPostFix1(exp) @ mapx(CmdPostFix1, seq1) @ mapx(CmdPostFix1, seq2) @ ["ITE"]
|   CmdPostFix1(WRITE(exp)) = ExpPostFix1(exp) @[ "WRITE"]
|   CmdPostFix1(READ(var)) = [ var,  "READ"];

fun SeqPostFix1(SEQ(CmdSeq)) = mapx(CmdPostFix1, CmdSeq);
fun evaluate (stk : CmdStackElement Stack) = 
    let 
        fun evaluator(stk, eval : string Stack) = 
            if (empty(stk)) then getVar(top(eval))
            else 
                let 
                    val x = top(stk);
                    val CMD (y) = x;
                    val pr = toString f eval;
                in
                    if (y = "PLUS") then 
                        let 
                            val a = top(eval); val yz = pop(eval);
                            val b = top(yz); val yz = pop(yz);
                            val c = plus(getPairInt(b, a));val yz = push(c, yz);
                        in 
                            evaluator(pop(stk), yz)
                        end
                    else if (y = "MINUS") then 
                        let 
                            val a = top(eval); val yz = pop(eval);
                            val b = top(yz); val yz = pop(yz);
                            val c = minus(getPairInt(b,a));val yz = push(c, yz);
                        in 
                            evaluator(pop(stk), yz)
                        end
                    else if (y = "TIMES") then 
                        let 
                            val a = top(eval); val yz = pop(eval);
                            val b = top(yz); val yz = pop(yz);
                            val c = times(getPairInt(b, a));val yz = push(c, yz);
                        in 
                            evaluator(pop(stk), yz)
                        end
                    else if (y = "DIV") then 
                        let 
                            val a = top(eval); val yz = pop(eval);
                            val b = top(yz); val yz = pop(yz);
                            val c = divide(getPairInt(b, a));val yz = push(c, yz);
                        in 
                            evaluator(pop(stk), yz)
                        end
                    else if (y = "MOD") then
                         let 
                            val a = top(eval); val yz = pop(eval);
                            val b = top(yz); val yz = pop(yz);
                            val c = modulo(getPairInt(b, a));val yz = push(c, yz);
                        in 
                            evaluator(pop(stk), yz)
                        end
                    else if (y = "AND") then
                         let 
                            val a = top(eval); val yz = pop(eval);
                            val b = top(yz); val yz = pop(yz);
                            val c = And(getPairBool(b, a));val yz = push(c, yz);
                        in 
                            evaluator(pop(stk), yz)
                        end
                    else if (y = "OR") then
                         let 
                            val a = top(eval); val yz = pop(eval);
                            val b = top(yz); val yz = pop(yz);
                            val c = or(getPairBool(b, a));val yz = push(c, yz);
                        in 
                            evaluator(pop(stk), yz)
                        end
                    else if (y = "LT") then
                         let 
                            val a = top(eval); val yz = pop(eval);
                            val b = top(yz); val yz = pop(yz);
                            val c = lt(getPairStr(b, a));val yz = push(c, yz);
                        in 
                            evaluator(pop(stk), yz)
                        end
                    else if (y = "LEQ") then
                         let 
                            val a = top(eval); val yz = pop(eval);
                            val b = top(yz); val yz = pop(yz);
                            val c = leq(getPairStr(b, a));val yz = push(c, yz);
                        in 
                            evaluator(pop(stk), yz)
                        end
                    else if (y = "EQ") then
                         let 
                            val a = top(eval); val yz = pop(eval);
                            val b = top(yz); val yz = pop(yz);
                            val c = eq(getPairStr(b, a));val yz = push(c, yz);
                        in 
                            evaluator(pop(stk), yz)
                        end
                    else if (y = "GEQ") then
                         let 
                            val a = top(eval); val yz = pop(eval);
                            val b = top(yz); val yz = pop(yz);
                            val c = geq(getPairStr(b, a));val yz = push(c, yz);
                        in 
                            evaluator(pop(stk), yz)
                        end
                    else if (y = "GT") then
                         let 
                            val a = top(eval); val yz = pop(eval);
                            val b = top(yz); val yz = pop(yz);
                            val c = gt(getPairStr(b, a));val yz = push(c, yz);
                        in 
                            evaluator(pop(stk), yz)
                        end
                    else if (y = "NOT") then 
                        let 
                            val a = top(eval); val yz = pop(eval);
                            val b = not(getVarBool(a));
                            val yz = push(b, yz);
                        in 
                            evaluator(pop(stk), yz)
                        end
                    else if (y = "UNARY") then 
                        let 
                            val a = top(eval); val yz = pop(eval);
                            val b = unary(getVarInt(a));
                            val yz = push(b, yz);
                        in 
                            evaluator(pop(stk), yz)
                        end
                    
                    else evaluator(pop(stk), push(y, eval))
                end
    in evaluator(stk, list2stack([]))
    end;

fun arrayToStringUtil(arr : int array, i) = if i = Array.length(arr) then "" else Int.toString(Array.sub(arr, i))^", "^arrayToStringUtil(arr, i+1)
fun arrayToString(arr : int array) = "[ "^arrayToStringUtil(arr, 0)^" ]";
fun idf(s: string) = s;
fun cmdListToString(x::xs) = cmdToString(x)^", "^cmdListToString(xs) 
|   cmdListToString([]) = ""
and cmdToString(CMD x) = x 
|   cmdToString(ITEstk(x, y, z)) = cmdListToString(x)^","^cmdListToString(y)^", "^cmdListToString(z)^", ITE"
|   cmdToString(WHstk(x,y)) = cmdListToString(x)^", "^cmdListToString(y)^", WH"
signature VMC = 
sig 
    val rules : string Stack * CmdStackElement Stack -> string Stack * int array * CmdStackElement Stack 
    val postfix : string -> ID list
    val execute : string -> string Stack * int array * CmdStackElement Stack 
    val toString : string Stack * int array * CmdStackElement Stack -> string
end 

structure Vmc :> VMC =
struct
    fun rules (V : string Stack, C : CmdStackElement Stack) = 
        let
            val x = top(C) ;
            (* val symbolTable = generateSymbolTable(s); *)
            fun rulesUtil(ITEstk(exp, CmdSeq1, CmdSeq2)) =   
                let 
                    val x = evaluate(list2stack(exp)) 
                in if x = "1" then (V, M , list2stack(CmdSeq1 @ stack2list(pop(C)))) 
                    else (V, M, list2stack(CmdSeq2 @ stack2list(pop(C))))
                end 
            |   rulesUtil (WHstk (exp, CmdSeq)) = 
                let 
                    val x = evaluate(list2stack(exp)) 
                in if x = "1" then (V, M, list2stack(CmdSeq @ stack2list(C)))
                    else (V, M, pop(C))
                end
            |   rulesUtil(CMD "PLUS") = 
                let 
                    val a = top(V); val yz = pop(V);
                    val b = top(yz); val yz = pop(yz);
                    val c = plus(getPairInt(b, a));val yz = push(c, yz);
                in (yz, M, pop(C) )
                end 
            |   rulesUtil(CMD "MINUS") = 
                let 
                    val a = top(V); val yz = pop(V);
                    val b = top(yz); val yz = pop(yz);
                    val c = minus(getPairInt(b, a));val yz = push(c, yz);
                in (yz, M, pop(C))
                end 
            |   rulesUtil(CMD "TIMES") = 
                let 
                    val a = top(V); val yz = pop(V);
                    val b = top(yz); val yz = pop(yz);
                    val c = times(getPairInt(b, a));val yz = push(c, yz);
                in (yz, M, pop(C) )
                end 
            |   rulesUtil(CMD "DIV") = 
                let 
                    val a = top(V); val yz = pop(V);
                    val b = top(yz); val yz = pop(yz);
                    val c = divide(getPairInt(b, a));val yz = push(c, yz);
                in (yz, M, pop(C) )
                end 
            |   rulesUtil(CMD "MOD") = 
                let 
                    val a = top(V); val yz = pop(V);
                    val b = top(yz); val yz = pop(yz);
                    val c = modulo(getPairInt(b, a));val yz = push(c, yz);
                in (yz, M, pop(C))
                end
            |   rulesUtil(CMD "OR") = 
                let 
                    val a = top(V); val yz = pop(V);
                    val b = top(yz); val yz = pop(yz);
                    val c = or(getPairBool(b, a));val yz = push(c, yz);
                in (yz, M, pop(C))
                end
            |   rulesUtil(CMD "AND") = 
                let 
                    val a = top(V); val yz = pop(V);
                    val b = top(yz); val yz = pop(yz);
                    val c = And(getPairBool(b, a));val yz = push(c, yz);
                in (yz, M, pop(C))
                end
            |   rulesUtil(CMD "LT") = 
                let 
                    val a = top(V); val yz = pop(V);
                    val b = top(yz); val yz = pop(yz);
                    val c = lt(getPairStr(b, a));val yz = push(c, yz);
                in (yz, M, pop(C))
                end
            |   rulesUtil(CMD "LEQ") = 
                let 
                    val a = top(V); val yz = pop(V);
                    val b = top(yz); val yz = pop(yz);
                    val c = leq(getPairStr(b, a));val yz = push(c, yz);
                in (yz, M, pop(C))
                end
            |   rulesUtil(CMD "EQ") = 
                let 
                    val a = top(V); val yz = pop(V);
                    val b = top(yz); val yz = pop(yz);
                    val c = eq(getPairStr(b, a));val yz = push(c, yz);
                in (yz, M, pop(C))
                end
            |   rulesUtil(CMD "GT") = 
                let 
                    val a = top(V); val yz = pop(V);
                    val b = top(yz); val yz = pop(yz);
                    val c = gt(getPairStr(b, a));val yz = push(c, yz);
                in (yz, M, pop(C))
                end
            |   rulesUtil(CMD "GEQ") = 
                let 
                    val a = top(V); val yz = pop(V);
                    val b = top(yz); val yz = pop(yz);
                    val c = geq(getPairStr(b, a));val yz = push(c, yz);
                in (yz, M, pop(C))
                end
            |   rulesUtil (CMD "NOT") = 
                let 
                    val a = top(V); val yz = pop(V);
                    val b = not(getVarBool(a));
                    val yz = push(b, yz);
                in (yz, M, pop(C))
                end
            |   rulesUtil (CMD "UNARY") = 
                let 
                    val a = top(V); val yz = pop(V);
                    val b = unary(getVar(a));
                    val yz = push(b, yz);
                in (yz, M, pop(C))
                end
            |   rulesUtil(CMD "SET") = 
                let 
                    val value = top(V); val yz = pop(V);
                    val var = top(yz); val yz = pop(yz);
                    val upd = if findTypeVar(var, symbolTable) = "int" then updateVariable(var, valOf(Int.fromString(getVarInt(value))), M) else updateVariable(var, valOf(Int.fromString(getVarBool(value))), M)
                in (yz, M, pop(C))
                end
            |   rulesUtil(CMD "READ") = 
                let 
                    val x = valOf(TextIO.inputLine TextIO.stdIn);
                    val var = top(V); val yz = pop(V);
                    val upd = updateVariable(var, valOf(Int.fromString(x)), M)
                in (yz, M, pop(C))
                end 
            |   rulesUtil(CMD "WRITE") = 
                let 
                    val exp = top(V); val yz = pop(V);
                    val output = print(getVar(exp) ^ "\n");
                in (yz, M, pop(C))
                end
            |   rulesUtil(CMD x) = (push(x, V), M, pop(C))
        in rulesUtil(x)
        end 
    fun postfix(s : string) =
        let 
            val cmdseq =  getCommandSeq(s : string) 
        in  
            SeqPostFix1(cmdseq)
        end

    fun execute(s : string) = 
        let 
            val symbolTable = generateSymbolTable(s);
            val C = MakeCmdStack(s);
            val V = FunStack.create;
            fun executeUtil(c : CmdStackElement Stack, v : string Stack) = 
                if (empty(c)) then (v, M, c) 
                else 
                let 
                    val (x, y, z) = rules(v, c)
                in executeUtil(z, x)
                end
        in executeUtil(C, V)
        end
    fun toString(V : string Stack, M : int array, C : CmdStackElement Stack) = 
        "(V: " ^( FunStack.toString idf V )^", "^" M : "^arrayToString(M)^", C :"^(FunStack.toString cmdToString C)^" )"
end

