%{ 
#include<stdio.h> 
#include<math.h>
extern int yylex();
extern int yyparse();
int printList(int list[],int size);
int appendFirst(int list[],int size,int val);
extern FILE *yyin;
extern FILE *yyout;
int flag=0;
int exit_flag=0;
int list_flag=0;
int binary_flag=0;
int listValues[999];
int listIndex=0;
int holdIndex=0;
%} 
%start START
%token CONCAT
%token SET
%token DEFFUN
%token TRUE 
%token FALSE
%token PLUS 
%token MINUS
%token DIV
%token MULT
%token DEFVAR
%token FOR
%token IF 
%token EQUAL
%token OP
%token CP
%token DBLMULT
%token OC
%token EXIT 
%token LOAD 
%token FLE
%token DISP
%token FLOAT_NUMBER
%token IDENTIFIER
%token LISTOP
%token CC
%token COMMA
%token COMMENT
%token AND
%token OR
%token NOT
%token APPEND
%token NUMBER
%token LESS
%token NIL
%token LIST
%token WHILE
%% 

START:  | INPUT{
        if(!flag && list_flag){
        	fprintf(yyout,"Result:"); 
            printList(listValues,listIndex); 
            listIndex=0;
            holdIndex=0;
            list_flag=0;
        }
        else if(!flag && binary_flag){
            if($$==1)
        	    fprintf(yyout,"Result:T",$$);
            else
                fprintf(yyout,"Result:NIL",$$);
            binary_flag=0;
		}
        else if(!flag){
        	fprintf(yyout,"Result:%d\n",$$); 
		    exit(1);
        }
		return 0;
		};

INPUT: EXPI | EXPLISTI | EXPB{ binary_flag=1; }
;

EXPI: OP DEFVAR IDENTIFIER EXPI CP { $$=$4; } 
| OP SET IDENTIFIER EXPI CP { $$=$4; } 
| OP PLUS EXPI EXPI CP { $$=$3+$4; } 
| OP MINUS EXPI EXPI CP { $$=$3-$4; } 
| OP MULT EXPI EXPI CP { $$=$3*$4; } 
| OP DBLMULT EXPI EXPI CP { $$=pow($3,$4); } 
| OP DIV EXPI EXPI CP { $$=$3/$4; } 
| OP IDENTIFIER EXPLISTI CP { 
    $$=$3; 
    list_flag=1;
}
| OP DEFFUN IDENTIFIER IDLIST EXPLISTI CP{
    $$=$5;
    list_flag=1;
}
| OP IF EXPB EXPLISTI CP { 
    $$=$3; 
    if(!$3){
        listIndex=0;
        listValues[0]=NULL;
    }
    list_flag=1;
}
| OP IF EXPB EXPLISTI EXPLISTI CP{ 
    $$=$3;
    if($3){
        listIndex=holdIndex;
    }
    else{
        listIndex=listIndex-holdIndex;
        for(int i=0;i<listIndex;++i){
            listValues[i]=listValues[holdIndex+i];
        }
    }
    list_flag=1;
}
| OP WHILE EXPB EXPLISTI CP{ 
    $$=$3; 
    if(!$3){
        listIndex=0;
        listValues[0]=NULL;
    }
    list_flag=1;
}
| OP FOR OP IDENTIFIER EXPI EXPI CP EXPLISTI CP{
    list_flag=1;
}
| OP LIST VALUES CP{
    $$=1; 
    list_flag=1;
}
| OP EXIT CP {
	exit_flag=1;
	printf("Bye.");
	return 0;
}
| OP LOAD OC FLE OC CP{ $$=1; }
| OP DISP EXPI CP{ $$=1; }
| IDENTIFIER { $$=1; }
| NUMBER { $$=$1; }
| COMMENT { 
    printf("\nCOMMENT"); 
    return 0;
};


EXPB:  OP AND EXPB EXPB CP { $$=$3&&$4; } 
| OP OR EXPB EXPB CP { $$=$3||$4; } 
| OP NOT EXPB CP { $$=!$3; } 
| OP EQUAL EXPB EXPB CP { $$=($3==$4); } 
| OP EQUAL EXPI EXPI CP { $$=($3==$4); } 
| OP LESS EXPI EXPI CP { $$=($3<$4); } 
| BinaryValue{$$=$1;};

EXPLISTI: OP CONCAT EXPLISTI EXPLISTI CP{
    $$=1; 
    list_flag=1;
}
| OP APPEND EXPI EXPLISTI CP{
    $$=1;
    //listValues[listIndex]=$3;
    ++listIndex;
    list_flag=1;
}
| LISTVALUE{$$=1;}
;

IDLIST: OP ILIST CP;

ILIST: ILIST IDENTIFIER | IDENTIFIER;

LISTVALUE: LISTOP VALUES CP{
    list_flag=1;
    if(holdIndex==0)
        holdIndex=listIndex;
}
| LISTOP CP {
    $$=0;
    list_flag=1;
    listIndex=0;
}
| NIL{$$=0;}
;

VALUES: VALUES NUMBER  {
    listValues[listIndex]=$2;
    ++listIndex;
}
| NUMBER {
    listValues[listIndex]=$1;
    ++listIndex;
}
| NIL{ $$=0; };

BinaryValue: TRUE { $$=1; }
| FALSE { $$=0; };

%% 
int main(int argc, char *argv[]){ 
    FILE *fin; 
    FILE *fout;
    fout=fopen("parsed.txt","a");
    if( argc==1 ){
		while(!exit_flag)	
            yyout=fout;
			yyparse();
    }
    
    else {
        fin = fopen(argv[1],"r"); 
        yyin=fin;
        yyout=fout;
		while(!exit_flag)
			yyparse();
    }
    return 0; 
}

int yyerror(const char * ch) 
{ 
	printf("\nSYNTAX_ERROR"); 
    flag=1;
    exit_flag=1;
}

int printList(int list[],int size){
    printf("(");
    for(int i=0;i<size;++i)
        if(i==size-1)
            printf("%d",list[i]);
        else
            printf("%d",list[i]);
    printf(")");
}