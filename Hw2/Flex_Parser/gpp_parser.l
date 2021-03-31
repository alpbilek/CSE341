%{ 
    #include<stdio.h> 
    #include "y.tab.h" 
    extern int yylval; 
    int c_count = 0; 
%}

COMMENT ";;".*
LIMITED_VALUE [1-9][0-9]*|"0"
IDENTIFIER [a-zA-Z][a-zA-Z0-9]*
FLE [a-zA-Z0-9]+"."[a-z]+
FLOATING_POINT [0-9]+"."[0-9]*

%%
{COMMENT} { return COMMENT; }
and { return AND; }
or { return OR; }
not { return NOT; }
equal { return EQUAL; }
less { return LESS; }
nil { return NIL; }
list { return LIST; }
append { return APPEND; }
concat { return CONCAT; }
set { return SET; }
deffun { return DEFFUN; }
defvar { return DEFVAR; }
for { return FOR; }
while { return WHILE; }
if { return IF; }
exit { return EXIT; }
load { return LOAD; }
disp { return DISP; }
true { return TRUE; }
false { return FALSE; }
"'(" { return LISTOP; }
"+" { return PLUS; }
"-" { return MINUS; }
"/" { return DIV; }
"*" { return MULT; }
"(" { return OP; }
")" { return CP;}
"**" { return DBLMULT; }
"\"" {if(!c_count%2)
        return OC;
    else
        return CC;
    ++c_count;}
"," { return COMMA; }
{LIMITED_VALUE} {
    yylval=atoi(yytext); 
    return NUMBER; 
}
{FLOATING_POINT} {
    yylval=atof(yytext); 
    return FLOAT_NUMBER; 
}
{IDENTIFIER} { return IDENTIFIER; }
{FLE} { return FLE; }
" "|"\t" { } 
"\n" {printf("%s\n",yytext); return 0; }
.	 {printf("SYNTAX ERROR %s IS NOT RECOGNIZED",yytext); return 0;} 
%%

int yywrap() 
{ 
return 1; 
}