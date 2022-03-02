%{
   #include <iostream>
   #include<stdio.h>
   #include<string.h>
   #include<string>
   #include <stdlib.h>
   #include<vector>
   using namespace std;
   extern int yylex();
        extern int yyparse();
        extern FILE * yyin;
   void yyerror(const char *msg);
   extern int currLine;
   int myError = 0;
   int otherError = 0;
   char *identToken;
   int numberToken;
   int productionID = 0;
   int wcnt = 0;
   int temp_count = 0;
   struct symbol{
	char* name;
	int val;
	char* type;
	symbol(char* n, int v, char*t){
		name = n;
		val = v;
		type = t;	
	}	
	symbol(){};
   };
   vector<symbol> symbolTable;
   vector<string> temps;
   symbol find(char*);
   bool isFind(symbol);
   string newtemp();
   vector<symbol> whilevector;
%}

%union {
  struct attribute{
	char* name;
	int index;
	char* type;
	int value;
	int size;
  }attribute;
  char* op_val;
  int numberval;
}

%start prog_start
%token BEGIN_PARAMS END_PARAMS BEGIN_LOCALS END_LOCALS BEGIN_BODY END_BODY
%token FUNCTION RETURN MAIN
%token L_SQUARE_BRACKET
%token R_SQUARE_BRACKET
%token INTEGER ARRAY OF
%token IF THEN ENDIF ELSE
%token WHILE DO BEGINLOOP ENDLOOP  CONTINUE
%token READ WRITE
%token AND OR NOT TRUE FALSE
%token SUB ADD MULT DIV MOD
%token EQ NEQ LT GT LTE GTE
%token SEMICOLON COLON COMMA L_PAREN R_PAREN ASSIGN
%token <numberval>NUMBER 
%token <op_val>IDENT
%type <attribute> prog_start
%type <attribute> functions
%type <attribute> function 
%type <attribute> function_ident
%type <attribute> ident
%type <attribute> declaration
%type <attribute> declarations
%type <attribute> statement
%type <attribute> statements
%type <attribute> expression
%type <attribute> multiplicative_expression
%type <attribute> term
%type <attribute> expressions
%type <attribute> comma_sep_expressions
%type <attribute> bool_exp
%type <attribute> relation_and_exp
%type <attribute> relation_exp
%type <attribute> comp
%type <attribute> var
%type <attribute> vars

%define parse.error verbose

%%

prog_start: 
	functions
		{};

functions: 
	/* epsilon */
		{}
	| function functions
		{};

function: function_ident
	SEMICOLON
	BEGIN_PARAMS declarations END_PARAMS
	BEGIN_LOCALS declarations END_LOCALS
	BEGIN_BODY statements END_BODY 
{
printf("endfunc\n");	
};

ident: 	IDENT
	{
	$$.name = $1;
	}
;


function_ident: FUNCTION ident{
	symbol temp($2.name,0,"function");
	if(!isFind(temp)) {
    		symbolTable.push_back(temp);
		printf("func %s\n", $2.name);
	} else {
    		printf("error: function %s already exists", $2.name);
	}
}
;

declarations: 
	/* epsilon */
		{}
	| declaration SEMICOLON declarations
		{};

declaration: 
	IDENT COLON INTEGER
{
	symbol temp;
	temp.name = $1;
	temp.type = "integer";
	if(!isFind(temp)) {
                symbolTable.push_back(temp);
		printf(". %s\n", $1);
		//printf("ident %s of type %s is pushed into vector\n", temp.name, temp.type);
        } else {
                yyerror("variable already exists");
        }
}
	| IDENT COLON ARRAY L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET OF INTEGER
{
	if($5 <= 0){
		const char* msg = "array canoot have size 0 or less";
		yyerror(msg);
	}
	symbol temp; 
	temp.name = $1;
	temp.type = "array";


	if(!isFind(temp)) {
                symbolTable.push_back(temp);
                printf(".[] %s, %d\n", $1,$5);
        } else {
		const char* msg = "array already exists\n";
                yyerror(msg);
        }
}

;

statement: 
	var ASSIGN expression
{
	symbol temp;
//printf("assign\n");
	temp.name = $1.name;
//printf("assign\n");
	temp.type = $1.type;


	if(isFind(temp)){
		$1.value = $3.value;
		int i =0;
		char* name;
		while(i<symbolTable.size()){ //while i is less than size of vector
                if(strcmp(symbolTable[i].name, $1.name) == 0){
                symbolTable[i].val = $3.value;
                break;
                }
                else{
                 i++;
                }
}
		//printf("%s assinged %d\n", $1.name, $3.value);
		//printf("%s = %d\n", $1.name, $1.value);
		printf("= %s, %s\n", $1.name, $3.name);
	}
	else{
		printf("%s of type %s not found in vector\n", temp.name,temp.type);
		const char* msg = "variable is not declared in scope\n";
		yyerror(msg);
	}		
}
	| IF bool_exp THEN 
{printf("?:= else, %s\n", $2.name);}
	statements 
{}
ENDIF
{
printf(": else\n");
printf(": endif\n");
}
	| IF bool_exp THEN 
{
printf("?:= else, %s\n", $2.name);
}
	statements ELSE 
{
printf(":= endif\n");
printf(": else\n");
}
	statements ENDIF
{
printf(": endif\n");
}


	| WHILE 
 		{printf(": while_loop_label\n");}
	bool_exp BEGINLOOP 
		{
//if predicate is true then we skip to while_loop_body
printf("?:= while_loop_body, %s\n", $3.name);
string temp = "while" + std::to_string(wcnt);
symbol w;
w.name =const_cast<char*>(temp.c_str());
whilevector.push_back(w);
//if not then we skip to while_loop_end_label
printf(":= while_loop_end_label\n");
printf(": while_loop_body\n");
		}

	statements 
		{}

	ENDLOOP
		{
printf(":= while_loop_label\n");
printf(": while_loop_end_label\n");
whilevector.pop_back();
		}


	| DO BEGINLOOP statements ENDLOOP WHILE bool_exp
		{}
	| READ vars
		{}
	| WRITE vars
{
	printf(".> %s\n", $2.name);
}
	| CONTINUE
{
if(!whilevector.empty())
printf(":= while_loop_label\n");
else{
const char* msg = "continue error: not in while loop";
        yyerror(msg);
}
}
	| RETURN expression
		{};
	
statements: 
	statement SEMICOLON/* epsilon */
	{}
	| statement SEMICOLON statements
		{};

expression: 
	multiplicative_expression
{
//printf("expression\n");
$$=$1;
}
	| multiplicative_expression ADD expression
{ 
	string temp= newtemp();
        int sum = $1.value + $3.value;
 	//printf("sum : %d\n", sum);
        printf(". %s\n", temp.c_str());
        printf("+ %s, %s, %s\n", temp.c_str(), $1.name, $3.name);
        $$.value = sum;
        $$.name = const_cast<char*>(temp.c_str());	    
}
	| multiplicative_expression SUB expression
{
	string temp= newtemp();
        int dif = $1.value - $3.value;
 	//printf("dif : %d\n", dif);
        printf(". %s\n", temp.c_str());
        printf("- %s, %s, %s\n", temp.c_str(), $1.name, $3.name);
        $$.value = dif;
        $$.name = const_cast<char*>(temp.c_str());
};

multiplicative_expression: 
	term
{
$$=$1;
}
	| term MULT multiplicative_expression
{
	string temp= newtemp(); 
        int product = $1.value*$3.value;
        //printf("%d * %d : %d\n", $1.value, $3.value, product); 
        
        printf(". %s\n", temp.c_str());
        
        printf("* %s, %s, %s\n", temp.c_str(), $1.name, $3.name);
        
        $$.value = product;
        $$.name = const_cast<char*>(temp.c_str());
 
}
	| term DIV multiplicative_expression
{
	string temp= newtemp(); 
	int divide = $1.value/$3.value;
	//printf("div: %d\n", divide); 

	printf(". %s\n", temp.c_str());

	printf("/ %s, %s, %s\n", temp.c_str(), $1.name, $3.name);

	$$.value = divide;
	$$.name = const_cast<char*>(temp.c_str());
}
	| term MOD multiplicative_expression
		{ }
		;

term: 
	var
{
	symbol temp;
	temp.name = $1.name;
	temp.type = $1.type;
	if(isFind(temp)){
		temp = find($1.name);
        	$$.name = temp.name;
		$$.type = temp.type;
		$$.value = temp.val;
	}
	else{
		const char* msg = "variable not declared in scope";
		yyerror(msg);
	}	
		
}
	| SUB var
		{ }
	| NUMBER		
{
	$$.value = $1;
	//std::sprintf($$.name, "%d", $1);
	$$.name = const_cast<char*>((std::to_string($1)).c_str());
}
	| SUB NUMBER
		{ }
	| L_PAREN expression R_PAREN
{ 
$$ = $2;
}
	| SUB L_PAREN expression R_PAREN
		{ }
	| ident L_PAREN expressions R_PAREN
{
symbol temp;
temp.name = $1.name;
if(!isFind(temp)){
	const char* msg = "function is not declared in scope";
	yyerror(msg);		
}
 
};

expressions: 
	/* epsilon */
		{}
	| comma_sep_expressions
		{};

comma_sep_expressions: 
	expression
		{}
	| expression COMMA comma_sep_expressions
		{};

bool_exp:
	relation_and_exp
		{$$ = $1;}
	| relation_and_exp OR bool_exp
		{};

relation_and_exp:
	relation_exp
		{$$ = $1;}
	| relation_exp AND relation_and_exp
		{};

relation_exp:
	expression comp expression
{
$$.name = const_cast<char*>(newtemp().c_str());
$$.value = 0;
printf(". %s\n", $$.name);
switch($2.value){

case 1:
	printf("== %s, %s, %s\n",$$.name,$1.name,$3.name);
	if($1.value == $2.value)
		$$.value = 1;
	else
		$$.value = 0;
	break;

case 2:
	printf("!= %s, %s, %s\n",$$.name,$1.name,$3.name);
	if($1.value != $2.value)
                $$.value = 1;
        else
                $$.value = 0;
	break;	

case 3:
	printf("< %s, %s, %s\n",$$.name,$1.name,$3.name);
	if($1.value < $2.value)
                $$.value = 1;
        else
                $$.value = 0;
	break; 

case 4:
	printf("> %s, %s, %s\n",$$.name,$1.name,$3.name);
	if($1.value > $2.value)
                $$.value = 1;
        else
                $$.value = 0;
	break;

case 5:
	printf("<= %s, %s, %s\n",$$.name,$1.name,$3.name);
	if($1.value <= $2.value)
                $$.value = 1;
        else
                $$.value = 0;
	break;

case 6:
	printf(">= %s, %s, %s\n",$$.name,$1.name,$3.name);
	if($1.value >= $2.value)
                $$.value = 1;
        else
                $$.value = 0;
	break;
default:
	printf("no comparison operator matched.");
	$$.value = 0;
	break;
}
}
	| NOT expression comp expression
		{}
	| TRUE
		{}
	| NOT TRUE
		{}
	| FALSE
		{}
	| NOT FALSE
		{}
	| L_PAREN bool_exp R_PAREN
		{}
	| NOT L_PAREN bool_exp R_PAREN
		{};

comp:
	EQ
		{$$.value = 1;}
	| NEQ
		{$$.value = 2;}
	| LT
		{$$.value = 3;}
	| GT
		{$$.value = 4;}
	| LTE
		{$$.value = 5;}
	| GTE
		{$$.value = 6;}
;

var:  ident
{
$$ = $1;
$$.type = "integer";
}

	| ident L_SQUARE_BRACKET expression R_SQUARE_BRACKET
{
symbol temp;
temp.name = $1.name;
if(isFind(temp)){
	char* identifier = $1.name;
	std::string str(identifier);
	string index = identifier + std::string("[") + std::to_string($3.value) + std::string("]");
	$$.name = const_cast<char*>(index.c_str());;
	$$.type = "array";
	$$.index = $3.value;
	temp.name = $$.name;
	temp.type = $$.type;
	symbolTable.push_back(temp);
}
else{
	const char* msg = "array not declared in scope";
	yyerror(msg);
}
};
vars:
	var
		{}
	| var COMMA vars
		{};
	

%%

string newtemp(){
	string temp = "_temp_" + std::to_string(temp_count);
	temps.push_back(temp); //to stop destructor from deleting strings
	temp_count++;	
	return temp;
}

int main(int argc, char **argv)
{
	yyparse();
   return 0;
}

symbol find(char* y){
	int i = 0;
	symbol found;
        while(i<symbolTable.size()){ //while i is less than size of vector
                if(strcmp(symbolTable[i].name, y) == 0){ //if element at index i in vector is equal to symbol we are looking for
                found = symbolTable[i];  //found in vector
		return found;
                }
                else{
                i++;    //keep iterating through vector
                }
        }
	return found;
}

bool isFind(symbol y){
	int i = 0;
	
	while(i<symbolTable.size()){ //while i is less than size of vector
		if(strcmp(symbolTable[i].name, y.name) == 0 && strcmp(symbolTable[i].type, y.type) == 0){ //if element at index i in vector is equal to symbol we are looking for
		return true;  //found in vector
		}
		else{
		i++;	//keep iterating through vector
		}
	}
	return false; //not found in vector
}

void yyerror(const char* msg)
{
   if(myError == 0)
   {
      printf("** Line %d: %s\n", currLine, msg);
      otherError = 1;
   }
   else
   {
      if(otherError == 1)
      {
         printf("   (%s)\n", msg);
         otherError = 0;
      }
   }
  myError = 0;
  exit(0);
}


