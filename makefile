run:
	ml-lex while.lex
	ml-yacc while.yacc
	sml loader.sml
clean:
	rm while.lex.sml 
	rm while.yacc.desc 
	rm while.yacc.sig 
	rm while.yacc.sml