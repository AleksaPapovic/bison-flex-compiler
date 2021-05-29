%{
  #include <stdio.h>
  #include <stdlib.h>
  #include "defs.h"
  #include "symtab.h"
  #include "codegen.h"

  int yyparse(void);
  int yylex(void);
  int yyerror(char *s);
  void warning(char *s);

  extern int yylineno;
  int out_lin = 0;
  char char_buffer[CHAR_BUFFER_LENGTH];
  int error_count = 0;
  int warning_count = 0;
  int var_num = 0;
  int fun_idx = -1;
  int fcall_idx = -1;
  int lab_num = 0;
  int ter_num = -1;
  int f_num =0;
  int fcall2= 0;
  int tip = 0;
  int tt_ret = 0;
  int param_num = 0;
  int argum_num = 0;
  unsigned argn[800];
  int check = 1;
  int  assig_idx =0;
  int id_inc=0;
  FILE *output;
  
  int para_i[100];
  int pusovani_a[200];
  int ugnjezdeni = 0;
  int niz_lab[200];
  int when_count = 0;
  int when_start;
  int when_literali[200];
  
  
%}

%union {
  int i;
  char *s;
}

%token <i> _TYPE
%token _IF
%token _ELSE
%token _RETURN
%token _RANGO
%token _PARA
%token _EN 
%token _CHECK
%token _WHEN
%token _THEN
%token _DEFAULT
%token <s> _ID
%token <s> _INT_NUMBER
%token <s> _UINT_NUMBER
%token _LPAREN
%token _RPAREN
%token _LBRACKET
%token _RBRACKET
%token _ASSIGN
%token _SEMICOLON
%token _COLON
%token _QMARK
%token <i> _AROP
%token <i> _RELOP
%token _ZAREZ
%token <s> _INC
%type <i> num_exp exp literal
%type <i> function_call argument rel_exp vars inc_statement var_assig argument_list when_literal when_list if_part terarni izraz when

%nonassoc ONLY_IF
%nonassoc _ELSE

%%

program
  : g_variable_list function_list
      {  
        if(lookup_symbol("main", FUN) == NO_INDEX)
          err("undefined reference to 'main'");
          
        for (int i=0; i< SYMBOL_TABLE_LENGTH ;i++)
        {  
          if( get_kind(i) == GVAR)
            {
              if(get_atr2(i) != 5 && get_atr2(i) != 15)
                 err("Variable not defined  %s \n", get_name(i));
            }
         }
      }
  ;

function_list
  : function
  | function_list function
  ;

function
  : _TYPE _ID
      {
        f_num++;
        param_num = 0;
        fun_idx = lookup_symbol($2, FUN);
        if(fun_idx == NO_INDEX) 
          fun_idx = insert_symbol($2, FUN, $1, NO_ATR, NO_ATR);
        else 
          err("redefinition of function '%s'", $2);
          

        code("\n%s:", $2);
        code("\n\t\tPUSH\t%%14");
        code("\n\t\tMOV \t%%15,%%14");
      }
    _LPAREN param_list _RPAREN body
      {
        for (int i=0; i< SYMBOL_TABLE_LENGTH ;i++)
        {  
          if( get_kind(i) == VAR)
          {
            if(get_atr2(i) != 5 && get_atr2(i) != 15)
               err("Variable not defined  %s \n", get_name(i));
          }
        }
        clear_symbols(fun_idx + 1);
        var_num = 0;
        
        code("\n@%s_exit:", $2);
        code("\n\t\tMOV \t%%14,%%15");
        code("\n\t\tPOP \t%%14");
        code("\n\t\tRET");
      }
      
  ;

param_list
  : /* empty */  { set_atr1(fun_idx, 0); }
  |  params
  ;

parameter
  : _TYPE _ID
      {
      if( $1 == VOID)
        err("Void is invalid type of argument");   
      insert_symbol($2, PAR, $1, ++param_num, 5);
      set_atr1(fun_idx, param_num);
      set_atr2(fun_idx, f_num);
      argn[f_num*fun_idx+param_num] = $1 ;
      }
  ;

params
  :  parameter
  |  params _ZAREZ parameter
  ;


body
  : _LBRACKET variable_list
      {
        if(var_num)
          code("\n\t\tSUBS\t%%15,$%d,%%15", 4*var_num);
        code("\n@%s_body:", get_name(fun_idx));
      }
    statement_list _RBRACKET
    {
      if( tt_ret ==0 && get_type(fun_idx) != 3)
          warning("No return in function with int or unsigned");
        else
          tt_ret = 0;
      }
  ;

g_variable_list
  : /* empty */
  | g_variable_list g_variable
  ;

g_variable
  : _TYPE _ID _SEMICOLON 
      {
        if( $1 == VOID)
        {
          err("Void is invalid type of global variable"); 
        }
        if(lookup_symbol($2, GVAR) == NO_INDEX){
        insert_symbol($2, GVAR, $1, NO_ATR, NO_ATR); 
         code("\n%s: ", $2);	
	  code("\n\t\t WORD 1");
	  }
         else
          err("redefinition of global variable'%s'", $2); 
        
      }
  ;
  
variable_list
  : /* empty */
  | variable_list variable
  ;

variable
  : _TYPE {tip = $1;} vars _SEMICOLON 
      {
        if( $1 == VOID)
        {
          err("Void is invalid type of variable"); 
        }
      }
  ;


vars
  :  var_assig
  | vars _ZAREZ var_assig 
  ;

var_assig
  : _ID 
      {
      if(lookup_symbol($1, VAR|PAR) == -1) 
        insert_symbol($1, VAR, tip, ++var_num, NO_ATR); 
        else
        err("redefinition of '%s'", $1); 
        }

  | _ID _ASSIGN num_exp 
      { 
       if(lookup_symbol($1, VAR|PAR) == -1) 
       {
        insert_symbol($1, VAR, tip, ++var_num, NO_ATR); 
        if( tip != get_type($3))
          err("incompatible types in assignment");
        int idx = lookup_symbol($1, VAR|PAR);
        set_atr2(idx,5);
       }
       else
        err("redefinition of '%s'", $1); 
         int idx1 = lookup_symbol($1, VAR|PAR);
         gen_mov($3, idx1);
      }
  ;

statement_list
  : /* empty */
  | statement_list statement
  ;

statement
  : compound_statement
  | assignment_statement
  | if_statement
  | return_statement {tt_ret = 1;}
  | inc_statement
  | para_statement
  | check_statement
  | func_statement
  ;
  
  para_statement
  : _PARA _LPAREN {
      $<i>$ = ++lab_num;
      code("\n@para%d:", lab_num);
      
  } _ID 
      {  
      unsigned idx = lookup_symbol($4, VAR|PAR);
      if(lookup_symbol($4, VAR|PAR) == -1) 
        err("Variable not defined  '%s'", $4); 
      else{
      set_atr2(idx,5);
      $<i>$ = idx;
      }
      }
      _EN _RANGO  literal 
      {
      gen_mov($8, $<i>5);
    
      code("\n@en%d:", $<i>3);
      } _COLON literal 
      {
      int lit_val1 = atoi(get_name($8));
      int lit_val2 = atoi(get_name($11));
      para_i[lab_num]=lit_val1;
      unsigned idx = lookup_symbol($4, VAR|PAR);
      int lit1_idx = lookup_symbol(get_name($8),  LIT);
      int lit2_idx = lookup_symbol(get_name($11),  LIT);
      
      
      if( lit_val1 < lit_val2){
        err("Const2 is greater than const1 in para statement");}
      else if(get_type(idx) != get_type(lit1_idx)){
        err("incompatible types in assignment");}
      else if(get_type(idx) != get_type(lit2_idx)){
        err("incompatible types in assignment");}
        else{
        gen_cmp($<i>5, $11);
        code("\n\t\t%s\t@endpara%d", opp_jumps[1 + (get_type(idx) - 1) * RELOP_NUMBER], $<i>3);
        }
      } 
      _RPAREN statement
      {
        code("\n\t\tSUBS\t");
        gen_sym_name($<i>5); 
        code(", $1, ");
        gen_sym_name($<i>5);
        code("\n\t\tJMP \t@en%d", $<i>3);
        code("\n@endpara%d:", $<i>3);
      }
  ;

check_statement
  : _CHECK _LPAREN
     {	  
     //when_start = when_count;
     $<i>$ = ++lab_num;
     code("\n@check%d:", lab_num);
     code("\n\t\tJMP\t@test%d", lab_num);
     }
   _ID 
      {
      check++;
      niz_lab[check] = lab_num;
      $<i>$ = get_last_element();
      unsigned idx = lookup_symbol($4, VAR|PAR|GVAR); 
      if(idx == -1) 
        err("Variable not defined  '%s'", $4); 
    }
      _RPAREN  _LBRACKET  when_list   { 
      code("\n@test%d:",  niz_lab[check]);
      unsigned idx = lookup_symbol($4, VAR|PAR|GVAR); 
      
      for (int i = 0; i < SYMBOL_TABLE_LENGTH; i++) {
          if (get_atr2(i) == niz_lab[check] && get_kind(i) == LIT)  {
          //printf("Labela je%d",niz_lab[check]);
            gen_cmp(idx, i);
            code("\n\t\tJEQ \t@when_list%s_%d",get_name(i),niz_lab[check]);
          }
        }
      } default_statement 
       _RBRACKET
      {
      unsigned idx = lookup_symbol($4, VAR|PAR);
      if( get_type(idx) != get_type($8) )
        err("Invalid type of check and when");
      else{
      code("\n@exit%d:",  niz_lab[check]);
      clear_symbols( $<i>5 + 1 ); 
      check--;
      }
      }
  ;

default_statement
  : /*no default*/
  | _DEFAULT _THEN { code("\n@default_%d:",  niz_lab[check]);}
  statement {code("\n\t\tJMP\t@exit%d",  niz_lab[check]);  }
  ;


  when_list
  : when {$$ = $1;}
  | when_list when {$$ = $2;
  if(get_type($1) != get_type($2))
    err("Invalid types of when");
  code("\n\t\tJMP\t@exit%d",  niz_lab[check]);
  } 
  ;
  
  when
  :_WHEN  when_literal _THEN {
  	 code("\n@when_list%d_%d:",atoi(get_name($2)), niz_lab[check]);
  	 // when_count++;
      }  statement
      {
     
      $$ = $2;
      }
      ;
  
  

when_literal
  : _INT_NUMBER
      {
      int x =lookup_symbol($1, LIT) ;
      if(lookup_symbol($1, LIT) != NO_INDEX && get_atr2(x) == niz_lab[check])
      {
        err("redefinition of when_literal '%s'", $1);
      }
      else
      {
      	when_literali[when_count] = x;
        $$ =  insert_symbol($1, LIT, INT, NO_ATR,niz_lab[check]);
       
      }
      }
  | _UINT_NUMBER
      {
      int x =lookup_symbol($1, LIT) ;
      if(lookup_symbol($1, LIT) != NO_INDEX && get_atr2(x) == niz_lab[check])
      {
        err("redefinition of when_literal '%s'", $1);
      }
      else
      {
        when_literali[when_count] = x;
          $$ = insert_symbol($1, LIT, UINT, NO_ATR,niz_lab[check]);
      }
      }
   ;
   
  inc_statement
  : _ID _INC _SEMICOLON
      {      
      int idx1= lookup_symbol($1, FUN);
      if(idx1 != NO_INDEX)
        err("function '%s' is invalid increment operand ", $1);
     int idx2 = lookup_symbol($1, VAR|PAR|GVAR);
      if(idx2 == NO_INDEX)
        err("'%s' undeclared", $1);
        code("\n\t\t\tADDS\t"); 
        gen_sym_name(idx2); 
        code(", $1, ");
        gen_sym_name(idx2);
      }
  ;
  
compound_statement
  : _LBRACKET statement_list _RBRACKET
  ;

assignment_statement
  : _ID {assig_idx =0;} _ASSIGN num_exp _SEMICOLON
      { 
	 int idx = lookup_symbol($1, VAR|PAR|GVAR); 
      if(  assig_idx == 1)
        err("Variable not defined  %s \n", get_name(idx));  
      else
        set_atr2(idx,5);   
      assig_idx = 0;      
      
       
        if(idx == NO_INDEX)
          err("invalid lvalue '%s' in assignment", $1);
          if(get_type(idx) != get_type($4))
            err("incompatible types in assignment");
		else{

            gen_mov($4, idx);
            
             if( get_atr2(idx) == 10|| get_atr2(idx) == 15){
             		set_atr2(idx, get_atr2(idx)-10);
             }
             for (int i=0; i< SYMBOL_TABLE_LENGTH ;i++){  
               if( (get_atr2(i) == 10 || get_atr2(i) == 15) && 
               get_kind(i) != LIT && get_kind(i) != FUN){
             	int t1 = get_type(i);    
             	code("\n\t\t%s\t", ar_instructions[(t1 - 1) * AROP_NUMBER]);
             	gen_sym_name(i);
             	code(", $1, ");
             	gen_sym_name(i);
             	free_if_reg(i);
             	set_atr2(i, get_atr2(i)-10);
         	}
             }
        }
      }
  ;

num_exp
  : exp

  | num_exp _AROP exp
      {
        if(get_type($1) != get_type($3))
          err("invalid operands: arithmetic operation");
	else{       
        int t1 = get_type($1);    
        code("\n\t\t%s\t", ar_instructions[$2 + (t1 - 1) * AROP_NUMBER]);
        gen_sym_name($1);
        code(",");
        gen_sym_name($3);
        code(",");
        free_if_reg($3);
        free_if_reg($1);
        $$ = take_reg();
        gen_sym_name($$);
        set_type($$, t1);
        }
      }
  ;

exp
  : literal
  | terarni
  | _ID
      {
        $$ = lookup_symbol($1, VAR|PAR|GVAR);
        if($$ == NO_INDEX)
          err("'%s' undeclared", $1);
      }
  | _ID _INC
      {
      $$ = lookup_symbol($1, VAR|PAR|GVAR);
      if($$ == NO_INDEX)
        err("'%s' undeclared", $1);
      int idx1 = lookup_symbol($1, FUN);
      if(idx1 != NO_INDEX)
        err("function '%s' is invalid increment operand ", $1);
        
      int v_id = lookup_symbol($1, VAR|PAR|GVAR);
      if( get_atr2( v_id) !=5 && v_id != NO_INDEX) 
      {
       err("Variable not defined %s \n", get_name(v_id));
       assig_idx = 1;
     }
    if(get_atr2($$)== 5)
     set_atr2($$,15);
    else
    set_atr2($$,10);
      }
  | function_call
      {
      assig_idx = 0;
       $$ = take_reg();
       gen_mov(FUN_REG, $$);
      }
  
  | _LPAREN num_exp _RPAREN
      { $$ = $2; }
  ;

literal
  : _INT_NUMBER
      { $$ = insert_literal($1, INT); }

  | _UINT_NUMBER
      { $$ = insert_literal($1, UINT); }
  ;

function_call
  : _ID 
      {
        fcall_idx = lookup_symbol($1, FUN);
        fcall2 = lookup_symbol($1, FUN);
      	int f_num2 = get_atr2( fcall_idx );
      	fcall2 = f_num2  * fcall2;
        if(fcall_idx == NO_INDEX)
          err("'%s' is not a function", $1);
      }
       _LPAREN argument_list {
    
    for(int j=argum_num-1;  j >=0; j--)
    {
    code("\n\t\t\tPUSH\t");
    gen_sym_name(pusovani_a[j]); 
    }
    } _RPAREN
      {
       if(get_atr1(fcall_idx) != $4)
          err("wrong number of args to function '%s'", get_name(fcall_idx));
       else{
        code("\n\t\t\tCALL\t%s", get_name(fcall_idx));
        if($4 > 0)
          code("\n\t\t\tADDS\t%%15,$%d,%%15", $4 * 4);        
         set_type(FUN_REG, get_type(fcall_idx));
         $$ = FUN_REG;
         argum_num=0;
         }
      }
  ;

func_statement
  : function_call _SEMICOLON
  ;

  
  argument_list
  : /* empty */ { $$ = 0; }
  | arguments { $$ = argum_num;}
  ;

arguments
  : argument 
  | arguments _ZAREZ argument 
  ;

argument
  : num_exp
      { 
      if(argn[fcall2 + argum_num+1]!= get_type($1)){
          err("incompatible type for argument in '%s'",get_name(fcall_idx));}
      else{
      free_if_reg($1);
      unsigned arg_index = lookup_symbol(get_name($1), LIT|VAR|PAR|GVAR);
      if(arg_index != -1){
      pusovani_a[argum_num]=  arg_index;
      }
      else{
      pusovani_a[argum_num] = $1;
      }
      ++argum_num;
      }
      }
  ;

if_statement
  : if_part %prec ONLY_IF
      { code("\n@exit%d:", $1); }

  | if_part _ELSE statement
      { code("\n@exit%d:", $1); }
  ;

if_part
  : _IF _LPAREN
      {
        $<i>$ = ++lab_num;
        code("\n@if%d:", lab_num);
      }
    rel_exp
      {
        code("\n\t\t%s\t@false%d", opp_jumps[$4], $<i>3);
        code("\n@true%d:", $<i>3);
      }
    _RPAREN statement
      {
        code("\n\t\tJMP \t@exit%d", $<i>3);
        code("\n@false%d:", $<i>3);
        $$ = $<i>3;
      }
  ;

rel_exp
  : num_exp _RELOP num_exp
      {
        if(get_type($1) != get_type($3))
          err("invalid operands: relational operator");
        $$ = $2 + ((get_type($1) - 1) * RELOP_NUMBER);
        gen_cmp($1, $3);
      }
  ;

terarni
  : _LPAREN rel_exp _RPAREN _QMARK izraz _COLON izraz
	{
		if(get_type($5) != get_type($7))
		{
			err("Tipovi izraza se ne poklapaju!");
		}
		$$ = take_reg();
		++ter_num;
		code("\n@terarni%d:", ter_num);
		code("\n\t\t%s\t@false%d", opp_jumps[$2],ter_num);
		code("\n@true%d:", ter_num);
		gen_mov($5, $$);
	  	code("\n\t\t\tJMP \t@exit%d", ter_num);
    		code("\n@false%d:", ter_num);
		gen_mov($7, $$);
		code("\n@exit%d:", ter_num);
		
	}
	;

izraz
	: _ID
	{
	$$ = lookup_symbol($1, (VAR|PAR|GVAR));
    	if($$ == -1)
          err("'%s' undeclared", $1);
	}
  	| literal
  	{
  	$$ = $1;
  	}
	;

return_statement
  : _RETURN num_exp _SEMICOLON
      {
       if(get_type(fun_idx) == 3)
      {
        err("Return in void");
      }
      if(get_type(fun_idx) != get_type($2))
        err("incompatible types in return");
      gen_mov($2, FUN_REG);
      code("\n\t\tJMP \t@%s_exit", get_name(fun_idx));        
      }
 |  _RETURN _SEMICOLON 
      { 
      if(get_type(fun_idx) != 3)
        warning("Return is blank");
       code("\n\t\tJMP \t@%s_exit", get_name(fun_idx));    
      }
  ;

%%

int yyerror(char *s) {
  fprintf(stderr, "\nline %d: ERROR: %s", yylineno, s);
  error_count++;
  return 0;
}

void warning(char *s) {
  fprintf(stderr, "\nline %d: WARNING: %s", yylineno, s);
  warning_count++;
}

int main() {
  int synerr;
  init_symtab();
  output = fopen("output.asm", "w+");

  synerr = yyparse();

  clear_symtab();
  fclose(output);
  
  if(warning_count)
    printf("\n%d warning(s).\n", warning_count);

  if(error_count) {
    remove("output.asm");
    printf("\n%d error(s).\n", error_count);
  }

  if(synerr)
    return -1;  //syntax error
  else if(error_count)
    return error_count & 127; //semantic errors
  else if(warning_count)
    return (warning_count & 127) + 127; //warnings
  else
    return 0; //OK
}

