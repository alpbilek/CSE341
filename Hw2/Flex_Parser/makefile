flex:	gpp_parser.l
	lex gpp_parser.l
	yacc -d gpp_parser.y
	gcc lex.yy.c y.tab.c -w -lm -o gpp_parser.out


clean:
	rm *.out
