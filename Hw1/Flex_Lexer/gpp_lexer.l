%{ 
	
    int c_count = 0; 
    #define AND "KW_AND"
    #define OR "KW_OR"
    #define NOT "KW_NOT"
    #define EQUAL "KW_EQUAL"
    #define LESS "KW_LESS"
    #define NIL "KW_NIL"
    #define LIST "KW_LIST"
    #define APPEND "KW_APPEND"
    #define CONCAT "KW_CONCAT"
    #define SET "KW_SET"
    #define DEFFUN "KW_DEFFUN"
    #define FOR "KW_FOR"
    #define IF "KW_IF"
    #define EXIT "KW_EXIT"
    #define LOAD "KW_LOAD"
    #define DISP "KW_DISP"
    #define TRUE "KW_TRUE"
    #define FALSE "KW_FALSE"
    #define PLUS "OP_PLUS"
    #define MINUS "OP_MINUS"
    #define DIV "OP_DIV"
    #define MULT "OP_MULT"
    #define OP "OP_OP"
    #define CP "OP_CP"
    #define DBLMULT "OP_DBLMULT"
    #define OC "OP_OC"
    #define CC "OP_CC"
    #define COMMA "OP_COMMA"
    #define VALUE "VALUE"
    #define COMMENT "COMMENT"
    #define ID "IDENTIFIER"
%}

COMMENT ";;".*
LIMITED_VALUE [1-9][0-9]*|"0"
IDENTIFIER [a-zA-Z][a-zA-Z0-9]*
FLOATING_POINT [0-9]+"."[0-9]*
EXIT "(exit)"
EXIT2 "\n\n"
%% 
{EXIT} {fprintf(yyout,"%s\n",OP);
    fprintf(yyout,"%s\n",EXIT);
    fprintf(yyout,"%s\n",CP);}
{COMMENT} {fprintf(yyout,"%s\n",COMMENT);}
{EXIT2} {return 0;}
and {fprintf(yyout,"%s\n",AND);}
or {fprintf(yyout,"%s\n",OR);}
not {fprintf(yyout,"%s\n",NOT);}
equal {fprintf(yyout,"%s\n",EQUAL);}
less {fprintf(yyout,"%s\n",LESS);}
nil {fprintf(yyout,"%s\n",NIL);}
list {fprintf(yyout,"%s\n",LIST);}
append {fprintf(yyout,"%s\n",APPEND);}
concat {fprintf(yyout,"%s\n",CONCAT);}
set {fprintf(yyout,"%s\n",SET);}
deffun {fprintf(yyout,"%s\n",DEFFUN);}
for {fprintf(yyout,"%s\n",FOR);}
if {fprintf(yyout,"%s\n",IF);}
exit {fprintf(yyout,"%s\n",EXIT);}
load {fprintf(yyout,"%s\n",LOAD);}
disp {fprintf(yyout,"%s\n",DISP);}
true {fprintf(yyout,"%s\n",TRUE);}
false {fprintf(yyout,"%s\n",FALSE);}
"+" {fprintf(yyout,"%s\n",PLUS);}
"-" {fprintf(yyout,"%s\n",MINUS);}
"/" {fprintf(yyout,"%s\n",DIV);}
"*" {fprintf(yyout,"%s\n",MULT);}
"(" {fprintf(yyout,"%s\n",OP);}
")" {fprintf(yyout,"%s\n",CP);}
"**" {fprintf(yyout,"%s\n",DBLMULT);}
"\"" {if(!c_count%2)
        fprintf(yyout,"%s\n",OC);
    else
        fprintf(yyout,"%s\n",CC);
    ++c_count;}
"," {fprintf(yyout,"%s\n",COMMA);}
{LIMITED_VALUE}|{FLOATING_POINT} {fprintf(yyout,"%s\n",VALUE);}
{IDENTIFIER} {fprintf(yyout,"%s\n",ID);}
" "|"\t"|"\n" { } 
.	 {fprintf(yyout,"SYNTAX ERROR %s IS NOT RECOGNIZED",yytext); return 0;} 
%%
int yywrap();

int main(int argc, char *argv[]){
	FILE* fin;
	FILE* fout;
	fout=fopen("parsed_cpp.txt","a");
	if(argc==2){
		fin=fopen(argv[1],"r");
        yyin=fin;
        yyout=fout;
        yylex();
	}
	else{
		yyout=fout;
		yylex();
	}
}
