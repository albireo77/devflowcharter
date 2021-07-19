%{

uses 
   YaccLib, LexLib, Pascal_Generator, ParserHelper, Infrastructure;

var
   paramList: array of integer;
   arg1: TIdentInfo;
   pcount, slength, lOrigType, lType, rval: integer;
   lIsReal, lIsInteger, lIsEnum, is_constant: boolean;

%}

%token T_NOT T_OR T_AND T_DIV T_MOD	/* logical operator */
%token GL GE LE NE			/* arithmetic operator */
%token T_ASSIGN_SIGN 			/* assign */
%token T_DBLDOT				/* .. */
%token L_REAL L_BOOL L_HEX		/* number */
%token <String> T_IDENTIFIER		/* var, const and routine identifier */
%token <Integer> L_INT
%token <String> T_STRING			/* text string */
%token T_READLN T_WRITELN T_READ T_WRITE	/* input/output instruction */
%token ILLEGAL

/* predefined functions */
%token 	T_SIN 
	T_COS 
	T_TAN 
	T_COTAN 
	T_SQR 
	T_EXP 
	T_LN 
	T_ABS 
	T_SQRT 
	T_LEN 
	T_RAND 
	T_TRUNC 	 
	T_NEW 
	T_DISPOSE
	T_BREAK
	T_CONTINUE
	T_EXIT
	T_ASSIGN
	T_RESET
	T_REWRITE
	T_APPEND
	T_CLOSE
	T_EOF
	T_EOLN
	T_SETLEN
	T_NIL
	T_ORD
	T_CHR
%token	<String> T_PI

%type <Integer> statement
%type <Integer> table_exp
%type <Integer> routine
%type <Integer> output_statement
%type <String> var_const
%type <String> valid_identifier
%type <Integer> struct_exp

%start input_line	

%left GL GE LE NE '='
%left '+' '-' T_OR
%left '*' '/' T_AND
%nonassoc UMINUS

%%

input_line:			assignment	{ CheckMode([yymAssign]); }
			|	condition       { CheckMode([yymCondition]); }
			|	input           { CheckMode([yymInput]); }
			|	output          { CheckMode([yymOutput]); }
			|	case            { CheckMode([yymCaseValue]); }
			|	range           { CheckMode([yymFor, yymCase, yymCaseValue, yymReturn, yymVarSize]); }
			|	routine_call    { CheckMode([yymFuncCall, yymReturn]); }
;

routine_call:		T_BREAK			{
							if not TParserHelper.IsInLoop then
                                                        begin
                                                           yyerrmsg := i18Manager.GetString('BreakErr');
                                                           yyabort;
                                                        end;
						}

		|	T_EXIT

		|	T_CONTINUE		{
							if not TParserHelper.IsInLoop then
                                                        begin
                                                           yyerrmsg := i18Manager.GetString('ContErr');
                                                           yyabort;
                                                        end;

						}

		|	routine

		|	routine_call ';'							
;

assignment:		var_const T_ASSIGN_SIGN statement   {
                                                        arg1 := TParserHelper.GetIdentInfo($1);
                                                        if arg1.IdentType = CONSTANT then
                                                        begin
                                                           yyerrmsg := i18Manager.GetFormattedString('BadConstOp', [$1]);
                                                           yyabort;
                                                        end
							else if arg1.TType = PASCAL_TEXT_FILE_TYPE then
                                                        begin
                                                           yyerrmsg := i18Manager.GetFormattedString('BadVarOper', [$1]);
                                                           yyabort;
                                                        end
                                                        else if arg1.IdentType = ENUM_VALUE then
                                                        begin
                                                           yyerrmsg := i18Manager.GetFormattedString('BadEnumOper', [$1]);
                                                           yyabort;
                                                        end
							else if not TParserHelper.AreTypesCompatible(arg1.TType, $3) then
							begin
                                                           yyerrmsg := i18Manager.GetFormattedString('IncTypes', [arg1.TypeAsString, TParserHelper.GetTypeAsString($3)]);
                                                           yyabort;
                                                        end;
						}

		|	var_const '^' T_ASSIGN_SIGN statement	{
                                                        arg1 := TParserHelper.GetIdentInfo($1);
							if arg1.IdentType = CONSTANT then
                                                        begin
                                                           yyerrmsg := i18Manager.GetFormattedString('BadConstOp', [$1]);
                                                           yyabort;
                                                        end
                                                        else if not arg1.IsPointer then
                                                        begin
                                                           yyerrmsg := i18Manager.GetFormattedString('NotPtrType', [$1]);
                                                           yyabort;
                                                        end
							else if not TParserHelper.AreTypesCompatible(arg1.TypeOriginal, $4) then
                                                        begin
                                                           yyerrmsg := i18Manager.GetFormattedString('IncTypes', [arg1.TypeAsString, TParserHelper.GetTypeAsString($4)]);
                                                           yyabort;
                                                        end;
						}

		|	table_exp T_ASSIGN_SIGN statement  {
							if not TParserHelper.AreTypesCompatible($1, $3) then
                                                        begin
                                                           yyerrmsg := i18Manager.GetFormattedString('IncTypes', [TParserHelper.GetTypeAsString($1), TParserHelper.GetTypeAsString($3)]);
                                                           yyabort;
                                                        end;
						}

		|	struct_exp T_ASSIGN_SIGN statement  {
							if not TParserHelper.AreTypesCompatible($1, $3) then
                                                        begin
                                                           yyerrmsg := i18Manager.GetFormattedString('IncTypes', [TParserHelper.GetTypeAsString($1), TParserHelper.GetTypeAsString($3)]);
                                                           yyabort;
                                                        end;
						}

		|	var_const T_ASSIGN_SIGN condition	{
                                                        arg1 := TParserHelper.GetIdentInfo($1);
							if arg1.IdentType = CONSTANT then
                                                        begin
                                                           yyerrmsg := i18Manager.GetFormattedString('BadConstOp', [$1]);
                                                           yyabort;
                                                        end
							else if arg1.TType <> PASCAL_BOOL_TYPE then
                                                        begin
                                                           yyerrmsg := i18Manager.GetFormattedString('IncTypes', [arg1.TypeAsString, 'boolean']);
                                                           yyabort;
                                                        end;
						}

		|	assignment ';'

		|	assignment ';' assignment

		|
;

input:			T_READLN '(' list_read ')'

		|	T_READ '(' list_read ')'

		|	T_READLN '(' ')'

		|	T_READLN

		|	T_READ '(' ')'

		|	T_READ

		|	input ';'
;

list_read:		var_const		{
                                                        arg1 := TParserHelper.GetIdentInfo($1);
							if arg1.IdentType = CONSTANT then
                                                        begin
                                                           yyerrmsg := i18Manager.GetFormattedString('BadConstOp', [$1]);
                                                           yyabort;
                                                        end
							else if (arg1.TType = PASCAL_BOOL_TYPE) or arg1.IsPointer or arg1.isRecord or (arg1.Size > 1) then
                                                        begin
                                                           yyerrmsg := i18Manager.GetFormattedString('BadInOper', [$1]);
                                                           yyabort;
                                                        end;
						}

		|	var_const '^'		{
                                                        arg1 := TParserHelper.GetIdentInfo($1);
							if arg1.IdentType = CONSTANT then
                                                        begin
                                                           yyerrmsg := i18Manager.GetFormattedString('BadConstOp', [$1]);
                                                           yyabort;
                                                        end
							else if not arg1.IsPointer then
                                                        begin
                                                           yyerrmsg := i18Manager.GetFormattedString('NotPtrType', [$1]);
                                                           yyabort;
                                                        end
							else if arg1.Size > 1 then
                                                        begin
                                                           yyerrmsg := i18Manager.GetFormattedString('BadInOper', [$1]);
                                                           yyabort;
                                                        end;
						}

		|	table_exp		{
							if ($1 = PASCAL_BOOL_TYPE) or TParserHelper.IsPointerType($1) or TParserHelper.isRecordType($1) then
                                                        begin
                                                           yyerrmsg := i18Manager.GetFormattedString('BadInOper', ['']);
                                                           yyabort;
                                                        end;
						}

		|	list_read ',' list_read
;

output:			T_WRITELN '(' output_statement ')'

		|	T_WRITE '(' output_statement ')'

		|	T_WRITELN '(' ')'

		|	T_WRITE '(' ')'
	
		|	T_WRITELN

		|	T_WRITE		

		|	output ';'
;

statement:		L_INT			{       $$ := PASCAL_INT_TYPE; is_constant := true;	}

		|	L_REAL 			{       $$ := PASCAL_REAL_TYPE; is_constant := true; 	}

		|	L_BOOL			{       $$ := PASCAL_BOOL_TYPE; is_constant := true;	}

		|	T_NIL			{       $$ := GENERIC_PTR_TYPE; is_constant := true;	}

		|	var_const		{
                                                        arg1 := TParserHelper.GetIdentInfo($1);
							is_constant := arg1.IdentType = CONSTANT;
                                                        $$ := TParserHelper.EncodeArrayType(arg1.TType, arg1.DimensCount);
						}

		|	routine			{      	$$ := $1; is_constant := false;	}

		|	table_exp		{ 	$$ := $1; is_constant := false; }

		|	T_STRING		{
							is_constant := true;
							slength := Length($1)-2; 
							if slength <> 1 then
						  	   $$ := PASCAL_STRING_TYPE
							else
							   $$ := PASCAL_CHAR_TYPE;
						}

		|	L_HEX			{       $$ := PASCAL_INT_TYPE; is_constant := true;	}

		|	struct_exp		{      	$$ := $1; is_constant := false;			}

		|	T_PI			{       $$ := PASCAL_REAL_TYPE; is_constant := true;	}

		|	var_const '^'		{
                                                        arg1 := TParserHelper.GetIdentInfo($1);
							is_constant := false;
							if arg1.IdentType = CONSTANT then
                                                        begin
                                                           yyerrmsg := i18Manager.GetFormattedString('BadConstOp', [$1]);
                                                           yyabort;
                                                        end
							else if not arg1.IsPointer then
                                                        begin
                                                           yyerrmsg := i18Manager.GetFormattedString('NotPtrType', [$1]);
                                                           yyabort;
                                                        end
                                                        else if arg1.DimensCount > 0 then
                                                        begin
                                                           yyerrmsg := i18Manager.GetFormattedString('BadPtrArray', [$1]);
                                                           yyabort;
                                                        end
                                                        else
                                                           $$ := arg1.TypeOriginal;
						}

		|	'@' var_const		{
                                                        arg1 := TParserHelper.GetIdentInfo($2);
							is_constant := false;
							if arg1.IdentType = CONSTANT then
                                                        begin
                                                           yyerrmsg := i18Manager.GetFormattedString('BadConstOp', [$2]);
                                                           yyabort;
                                                        end
							else
							   $$ := arg1.TypePointer;
						}

		|	'@' table_exp		{       $$ := TParserHelper.GetPointerType($2); is_constant := false;	}

                |	'@' struct_exp		{       $$ := TParserHelper.GetPointerType($2); is_constant := false;	}

		|	'-' statement %prec UMINUS	{
							if not TParserHelper.IsNumericType($2) then
                                                        begin
                                                           yyerrmsg := i18Manager.GetFormattedString('BadOperArg', [TParserHelper.GetTypeAsString($2), '-']);
                                                           yyabort;
                                                        end;
							$$ := $2;
						}

		|	statement '+' statement	{
							if not TParserHelper.IsNumericType($1) or not TParserHelper.IsNumericType($3) then
                                                        begin
                                                           if ($1 = PASCAL_STRING_TYPE) and ($3 = PASCAL_STRING_TYPE) then
                                                           begin
                                                              $$ := PASCAL_STRING_TYPE;
                                                              exit;
                                                           end;
                                                           yyerrmsg := i18Manager.GetFormattedString('IncTypes', [TParserHelper.GetTypeAsString($1), TParserHelper.GetTypeAsString($3)]);
                                                           yyabort;
                                                        end;
							if TParserHelper.IsRealType($1) or TParserHelper.IsRealType($3) then
							   $$ := PASCAL_REAL_TYPE
							else
							   $$ := PASCAL_INT_TYPE;
						}

		|	statement '-' statement	{
							if not TParserHelper.IsNumericType($1) or not TParserHelper.IsNumericType($3) then
                                                        begin
                                                           yyerrmsg := i18Manager.GetFormattedString('IncTypes', [TParserHelper.GetTypeAsString($1), TParserHelper.GetTypeAsString($3)]);
                                                           yyabort;
                                                        end;
							if TParserHelper.IsRealType($1) or TParserHelper.IsRealType($3) then
							   $$ := PASCAL_REAL_TYPE
							else
							   $$ := PASCAL_INT_TYPE;
						}

		|	statement '*' statement	{
							if not TParserHelper.IsNumericType($1) or not TParserHelper.IsNumericType($3) then
                                                        begin
                                                           yyerrmsg := i18Manager.GetFormattedString('IncTypes', [TParserHelper.GetTypeAsString($1), TParserHelper.GetTypeAsString($3)]);
                                                           yyabort;
                                                        end;
							if TParserHelper.IsRealType($1) or TParserHelper.IsRealType($3) then
							   $$ := PASCAL_REAL_TYPE
							else
							   $$ := PASCAL_INT_TYPE;
						}

		|	statement '/' statement	{
							if not TParserHelper.IsNumericType($1) or not TParserHelper.IsNumericType($3) then
                                                        begin
                                                           yyerrmsg := i18Manager.GetFormattedString('IncTypes', [TParserHelper.GetTypeAsString($1), TParserHelper.GetTypeAsString($3)]);
                                                           yyabort;
                                                        end;
							$$ := PASCAL_REAL_TYPE;
						}

		|	'(' statement ')'       {       $$:=$2; }



		|	statement T_DIV statement	{
							if not TParserHelper.IsIntegerType($1) then
                                                        begin
                                                           yyerrmsg := i18Manager.GetFormattedString('BadOperArg', [TParserHelper.GetTypeAsString($1), 'div']);
                                                           yyabort;
                                                        end
							else if not TParserHelper.IsIntegerType($3) then
                                                        begin
                                                           yyerrmsg := i18Manager.GetFormattedString('BadOperArg', [TParserHelper.GetTypeAsString($3), 'div']);
                                                           yyabort;
                                                        end;
							$$ := PASCAL_INT_TYPE;
						}

		|	statement T_MOD statement	  {
							if not TParserHelper.IsIntegerType($1) then
                                                        begin
                                                           yyerrmsg := i18Manager.GetFormattedString('BadOperArg', [TParserHelper.GetTypeAsString($1), 'mod']);
                                                           yyabort;
                                                        end
							else if not TParserHelper.IsIntegerType($3) then
                                                        begin
                                                           yyerrmsg := i18Manager.GetFormattedString('BadOperArg', [TParserHelper.GetTypeAsString($3), 'mod']);
                                                           yyabort;
                                                        end;
							$$ := PASCAL_INT_TYPE;
						}
;

condition:		T_NOT condition

		|	condition  T_AND  condition

		|	condition  T_OR  condition

		|	statement '=' statement	{
							if PASCAL_TEXT_FILE_TYPE in [$1, $3] then
                                                        begin
                                                           yyerrmsg := i18Manager.GetString('BadCmpr');
                                                           yyabort;
                                                        end
							else if $1 <> $3 then
                                                        begin
                                                           if (TParserHelper.IsNumericType($1) and TParserHelper.IsNumericType($3)) or
                                                              (TParserHelper.IsPointerType($1) and ($3 = GENERIC_PTR_TYPE)) or
                                                              (TParserHelper.IsPointerType($3) and ($1 = GENERIC_PTR_TYPE)) or
							      (($1 = PASCAL_CHAR_TYPE) and (($3 = PASCAL_STRING_TYPE) and (slength = 1))) or
							      (($3 = PASCAL_CHAR_TYPE) and (($1 = PASCAL_STRING_TYPE) and (slength = 1))) then exit;
                                                           yyerrmsg := i18Manager.GetFormattedString('IncTypes', [TParserHelper.GetTypeAsString($1), TParserHelper.GetTypeAsString($3)]);
                                                           yyabort;
                                                        end;
						}

		 |	statement GL statement	{
							if not TParserHelper.IsNumericType($1) or not TParserHelper.IsNumericType($3) then
                                                        begin
                                                           yyerrmsg := i18Manager.GetString('BadCmpr');
                                                           yyabort;
                                                        end;
						}
	
		 |	statement GE statement	{
							if not TParserHelper.IsNumericType($1) or not TParserHelper.IsNumericType($3) then
                                                        begin
                                                           yyerrmsg := i18Manager.GetString('BadCmpr');
                                                           yyabort;
                                                        end;
						}

		 |	statement LE statement	{
							if not TParserHelper.IsNumericType($1) or not TParserHelper.IsNumericType($3) then
                                                        begin
                                                           yyerrmsg := i18Manager.GetString('BadCmpr');
                                                           yyabort;
                                                        end;
						}

		 |	statement NE statement	{
							if PASCAL_TEXT_FILE_TYPE in [$1, $3] then
                                                        begin
                                                           yyerrmsg := i18Manager.GetString('BadCmpr');
                                                           yyabort;
                                                        end
							else if $1 <> $3 then
                                                        begin
                                                           if (TParserHelper.IsNumericType($1) and TParserHelper.IsNumericType($3)) or
                                                              (TParserHelper.IsPointerType($1) and ($3 = GENERIC_PTR_TYPE)) or
                                                              (TParserHelper.IsPointerType($3) and ($1 = GENERIC_PTR_TYPE)) or
							      (($1 = PASCAL_CHAR_TYPE) and (($3 = PASCAL_STRING_TYPE) and (slength = 1))) or
							      (($3 = PASCAL_CHAR_TYPE) and (($1 = PASCAL_STRING_TYPE) and (slength = 1))) then exit;
                                                           yyerrmsg := i18Manager.GetFormattedString('IncTypes', [TParserHelper.GetTypeAsString($1), TParserHelper.GetTypeAsString($3)]);
                                                           yyabort;
                                                        end;
						}

		|	'(' condition ')'
;

valid_identifier:	T_IDENTIFIER		{
							if not TParserHelper.IsDeclared($1) then
                                                        begin
                                                           yyerrmsg := i18Manager.GetFormattedString('UnknId', [$1]);
                                                           yyabort;
                                                        end;
						    	$$ := $1;
						}
;


var_const:		valid_identifier	{
                                                        arg1 := TParserHelper.GetIdentInfo($1);
							if not (arg1.IdentType in [VARIABLE, VARRAY, CONSTANT, ENUM_VALUE]) then
                                                        begin
                                                           yyerrmsg := i18Manager.GetFormattedString('UnknId', [$1]);
                                                           yyabort;
                                                        end;
							$$ := $1;
						}

;

routine:		valid_identifier '(' parameters_list ')'	{
                                                        arg1 := TParserHelper.GetIdentInfo($1);
							if arg1.IdentType <> ROUTINE_ID then
                                                        begin
                                                           yyerrmsg := i18Manager.GetFormattedString('UnknId', [$1]);
                                                           yyabort;
                                                        end
							else if not TParserHelper.ValidateUserFunctionParms($1, paramList) then
                                                        begin
                                                           yyerrmsg := i18Manager.GetFormattedString('BadFuncParms', [$1]);
                                                           yyabort;
                                                        end;
                                                        paramList := nil;
							$$ := arg1.TType;
						}

		|	T_NEW '(' statement ')'	{
							if not TParserHelper.IsPointerType($3) then
                                                        begin
                                                           yyerrmsg := i18Manager.GetFormattedString('BadFuncParmU', ['New']);
                                                           yyabort;
                                                        end;
							$$ := UNKNOWN_TYPE;
						}

		|	T_DISPOSE '(' statement ')'	{
							if not TParserHelper.IsPointerType($3) then
                                                        begin
                                                           yyerrmsg := i18Manager.GetFormattedString('BadFuncParmU', ['Dispose']);
                                                           yyabort;
                                                        end;
							$$ := UNKNOWN_TYPE;
						}

		|	T_SIN '(' statement ')'	{
							if not TParserHelper.IsNumericType($3) then
                                                        begin
                                                           yyerrmsg := i18Manager.GetFormattedString('BadFuncParm', [TParserHelper.GetTypeAsString($3), 'Sin']);
                                                           yyabort;
                                                        end;
                                        		$$ := PASCAL_REAL_TYPE;
						}

		|	T_COS '(' statement ')'	{
							if not TParserHelper.IsNumericType($3) then
                                                        begin
                                                           yyerrmsg := i18Manager.GetFormattedString('BadFuncParm', [TParserHelper.GetTypeAsString($3), 'Cos']);
                                                           yyabort;
                                                        end;
                                        		$$ := PASCAL_REAL_TYPE;
						}

		|	T_TAN '(' statement ')'	{
							if not TParserHelper.IsNumericType($3) then
                                                        begin
                                                           yyerrmsg := i18Manager.GetFormattedString('BadFuncParm', [TParserHelper.GetTypeAsString($3), 'Tan']);
                                                           yyabort;
                                                        end;
                                        		$$ := PASCAL_REAL_TYPE;
						}

		|	T_COTAN '(' statement ')'	{
							if not TParserHelper.IsNumericType($3) then
                                                        begin
                                                           yyerrmsg := i18Manager.GetFormattedString('BadFuncParm', [TParserHelper.GetTypeAsString($3), 'Cotan']);
                                                           yyabort;
                                                        end;
                                        		$$ := PASCAL_REAL_TYPE;
						}

		|	T_ABS '(' statement ')'	{
							if not TParserHelper.IsNumericType($3) then
                                                        begin
                                                           yyerrmsg := i18Manager.GetFormattedString('BadFuncParm', [TParserHelper.GetTypeAsString($3), 'Abs']);
                                                           yyabort;
                                                        end;
							if TParserHelper.IsRealType($3) then
                                        		   $$ := PASCAL_REAL_TYPE
							else
							   $$ := PASCAL_INT_TYPE;
						}

		|	T_SQRT '(' statement ')'	{
							if not TParserHelper.IsNumericType($3) then
                                                        begin
                                                           yyerrmsg := i18Manager.GetFormattedString('BadFuncParm', [TParserHelper.GetTypeAsString($3), 'Sqrt']);
                                                           yyabort;
                                                        end;
                                        		$$ := PASCAL_REAL_TYPE;
						}

		|	T_LN '(' statement ')'	{
							if not TParserHelper.IsNumericType($3) then
                                                        begin
                                                           yyerrmsg := i18Manager.GetFormattedString('BadFuncParm', [TParserHelper.GetTypeAsString($3), 'Ln']);
                                                           yyabort;
                                                        end;
							$$ := PASCAL_REAL_TYPE;
						}

		|	T_EXP '(' statement ')'	{
							if not TParserHelper.IsNumericType($3) then
                                                        begin
                                                           yyerrmsg := i18Manager.GetFormattedString('BadFuncParm', [TParserHelper.GetTypeAsString($3), 'Exp']);
                                                           yyabort;
                                                        end;
							$$ := PASCAL_REAL_TYPE;
						}

		|	T_LEN '(' statement ')'	{
							if $3 <> PASCAL_STRING_TYPE then
                                                        begin
                                                           yyerrmsg := i18Manager.GetFormattedString('BadFuncParm', [TParserHelper.GetTypeAsString($3), 'Length']);
                                                           yyabort;
                                                        end;
							$$ := PASCAL_INT_TYPE;
						}

		|	T_LEN '(' var_const ')'	{
                                                        arg1 := TParserHelper.GetIdentInfo($3);
							if (arg1.DimensCount = 0) and (arg1.TType <> PASCAL_STRING_TYPE) then
                                                        begin
                                                           yyerrmsg := i18Manager.GetFormattedString('BadFuncParmU', ['Length']);
                                                           yyabort;
                                                        end;
							$$ := PASCAL_INT_TYPE;
						}

		|	T_RAND '(' '-' L_INT ')'	{
							rand_flag := 1;
							yyerrmsg := i18Manager.GetFormattedString('BadFuncParmU', ['random']);
							yyabort;
						}

		|	T_RAND '(' statement ')'	{
							rand_flag := 1;
							if not TParserHelper.IsIntegerType($3) then
                                                        begin
                                                           yyerrmsg := i18Manager.GetFormattedString('BadFuncParm', [TParserHelper.GetTypeAsString($3), 'Random']);
                                                           yyabort;
                                                        end;
							$$ := PASCAL_INT_TYPE;
						}

		|	T_SQR '(' statement ')'	{
                                                        lIsInteger := TParserHelper.IsIntegerType($3);
							if not lIsInteger and not TParserHelper.IsRealType($3) then
                                                        begin
                                                           yyerrmsg := i18Manager.GetFormattedString('BadFuncParm', [TParserHelper.GetTypeAsString($3), 'Sqr']);
                                                           yyabort;
                                                        end;
							if lIsInteger then
							   $$ := PASCAL_INT_TYPE
							else
							   $$ := PASCAL_REAL_TYPE;
						}

		|	T_TRUNC '(' statement ')'	{
							if not TParserHelper.IsRealType($3) then
                                                        begin
                                                           yyerrmsg := i18Manager.GetFormattedString('BadFuncParm', [TParserHelper.GetTypeAsString($3), 'Trunc']);
                                                           yyabort;
                                                        end;
							$$ := PASCAL_INT_TYPE;
						}

		|	T_ASSIGN '(' var_const ',' statement ')'	{
                                                        arg1 := TParserHelper.GetIdentInfo($3);
							if arg1.TType <> PASCAL_TEXT_FILE_TYPE then
                                                        begin
                                                           yyerrmsg := i18Manager.GetFormattedString('BadFuncParm', [arg1.TypeAsString, 'assign']);
                                                           yyabort;
                                                        end
							else if $5 <> PASCAL_STRING_TYPE then
                                                        begin
                                                           yyerrmsg := i18Manager.GetFormattedString('BadFuncParm', [TParserHelper.GetTypeAsString($5), 'Assign']);
                                                           yyabort;
                                                        end;
							$$ := UNKNOWN_TYPE;
						}

		|	T_CLOSE '(' var_const ')'	{
                                                        arg1 := TParserHelper.GetIdentInfo($3);
							if arg1.TType <> PASCAL_TEXT_FILE_TYPE then
                                                        begin
                                                           yyerrmsg := i18Manager.GetFormattedString('BadFuncParm', [arg1.TypeAsString, 'Close']);
                                                           yyabort;
                                                        end;
							$$ := UNKNOWN_TYPE;
						}

		|	T_RESET '(' var_const ')'	{
                                                        arg1 := TParserHelper.GetIdentInfo($3);
							if arg1.TType <> PASCAL_TEXT_FILE_TYPE then
                                                        begin
                                                           yyerrmsg := i18Manager.GetFormattedString('BadFuncParm', [arg1.TypeAsString, 'Reset']);
                                                           yyabort;
                                                        end;
							$$ := UNKNOWN_TYPE;
						}

		|	T_REWRITE '(' var_const ')'	{
                                                        arg1 := TParserHelper.GetIdentInfo($3);
							if arg1.TType <> PASCAL_TEXT_FILE_TYPE then
                                                        begin
                                                           yyerrmsg := i18Manager.GetFormattedString('BadFuncParm', [arg1.TypeAsString, 'Rewrite']);
                                                           yyabort;
                                                        end;
							$$ := UNKNOWN_TYPE;
						}

		|	T_APPEND '(' var_const ')'	{
                                                        arg1 := TParserHelper.GetIdentInfo($3);
							if arg1.TType <> PASCAL_TEXT_FILE_TYPE then
                                                        begin
                                                           yyerrmsg := i18Manager.GetFormattedString('BadFuncParm', [arg1.TypeAsString, 'Append']);
                                                           yyabort;
                                                        end;
							$$ := UNKNOWN_TYPE;
						}

		|	T_EOF '(' var_const ')'	{
                                                        arg1 := TParserHelper.GetIdentInfo($3);
							if arg1.TType <> PASCAL_TEXT_FILE_TYPE then
                                                        begin
                                                           yyerrmsg := i18Manager.GetFormattedString('BadFuncParm', [arg1.TypeAsString, 'Eof']);
                                                           yyabort;
                                                        end;
							$$ := PASCAL_BOOL_TYPE;
						}

		|	T_EOLN '(' var_const ')'	{
                                                        arg1 := TParserHelper.GetIdentInfo($3);
							if arg1.TType <> PASCAL_TEXT_FILE_TYPE then
                                                        begin
                                                           yyerrmsg := i18Manager.GetFormattedString('BadFuncParm', [arg1.TypeAsString, 'Eoln']);
                                                           yyabort;
                                                        end;
							$$ := PASCAL_BOOL_TYPE;
						}
						
		|	T_SETLEN '(' var_const ',' statement ')'	{
                                                        arg1 := TParserHelper.GetIdentInfo($3);
							if ((arg1.DimensCount = 0) and (arg1.TType <> PASCAL_STRING_TYPE)) or ((arg1.DimensCount > 0) and not ReplaceStr(arg1.SizeAsString, ' ', '').StartsWith('[]')) then
                                                        begin
                                                           yyerrmsg := i18Manager.GetString('SetLenDynArr');
                                                           yyabort;
                                                        end;
							if not TParserHelper.IsIntegerType($5) then
                                                        begin
                                                           yyerrmsg := i18Manager.GetFormattedString('BadFuncParm', [TParserHelper.GetTypeAsString($5), 'SetLength']);
                                                           yyabort;
                                                        end;
							$$ := UNKNOWN_TYPE;
						}

		|	T_ORD '(' statement ')'	{
							if ($3 <> PASCAL_CHAR_TYPE) and not TParserHelper.IsEnumType($3) then
                                                        begin
                                                           yyerrmsg := i18Manager.GetFormattedString('BadFuncParm', [TParserHelper.GetTypeAsString($3), 'Ord']);
                                                           yyabort;
                                                        end;
							$$ := PASCAL_INT_TYPE;
						}

		|	T_CHR '(' statement ')'	{
							if $3 <> PASCAL_INT_TYPE then
                                                        begin
                                                           yyerrmsg := i18Manager.GetFormattedString('BadFuncParm', [TParserHelper.GetTypeAsString($3), 'Chr']);
                                                           yyabort;
                                                        end;
							$$ := PASCAL_CHAR_TYPE;
						}
;

parameters_list:	statement		{
                                                        paramList := nil;
							SetLength(paramList, 1);
							paramlist[0] := $1;
						}

		|	parameters_list ',' statement	{
							SetLength(paramList, Length(paramList)+1);
							paramList[High(paramList)] := $3;
						}

                |                               {       paramList := nil; }
;

struct_exp:		var_const '.' valid_identifier	{
                                                        arg1 := TParserHelper.GetIdentInfo($1);
							if not arg1.isRecord then
                                                        begin
                                                           yyerrmsg := i18Manager.GetFormattedString('NotStructType', [$1]);
                                                           yyabort;
                                                        end
							else if arg1.Size > 1 then
                                                        begin
                                                           yyerrmsg := i18Manager.GetFormattedString('BadVarOper', [$1]);
                                                           yyabort;
                                                        end
							else if TParserHelper.GetFieldType($1, $3) = NOT_DEFINED then
                                                        begin
                                                           yyerrmsg := i18Manager.GetFormattedString('NoFieldStruct', [arg1.TypeAsString, $3]);
                                                           yyabort;
                                                        end;
							$$ := TParserHelper.GetFieldType($1, $3);
						}

                |       var_const '^' '.' valid_identifier      {
                                                        arg1 := TParserHelper.GetIdentInfo($1);
                                                        lType := TParserHelper.GetFieldType($1, $4);
							if not TParserHelper.isRecordType(arg1.TypeOriginal) then
                                                        begin
                                                           yyerrmsg := i18Manager.GetFormattedString('NotStructType', [$1]);
                                                           yyabort;
                                                        end
                                                        else if not arg1.IsPointer then
                                                        begin
                                                           yyerrmsg := i18Manager.GetFormattedString('NotPtrType', [$1]);
                                                           yyabort;
                                                        end
							else if arg1.DimensCount > 0 then
                                                        begin
                                                           yyerrmsg := i18Manager.GetFormattedString('BadVarOper', [$1]);
                                                           yyabort;
                                                        end
							else if lType = NOT_DEFINED then
                                                        begin
                                                           yyerrmsg := i18Manager.GetFormattedString('NoFieldStruct', [TParserHelper.GetTypeAsString(arg1.TypeOriginal), $4]);
                                                           yyabort;
                                                        end;
							$$ := lType;
                                                }

		|	table_exp '.' valid_identifier	{
							if not TParserHelper.isRecordType($1) then
                                                        begin
                                                           yyerrmsg := i18Manager.GetFormattedString('BadVarOper', ['']);
                                                           yyabort;
                                                        end
							else if TParserHelper.GetFieldType($1, $3) = NOT_DEFINED then
                                                        begin
                                                           yyerrmsg := i18Manager.GetFormattedString('NoFieldStruct', [TParserHelper.GetTypeAsString($1), $3]);
                                                           yyabort;
                                                        end;
							$$ := TParserHelper.GetFieldType($1, $3);
						}

                |       struct_exp '^'          {
                                                        if not TParserHelper.IsPointerType($1) then
                                                        begin
                                                           yyerrmsg := i18Manager.GetFormattedString('IncTypes', [TParserHelper.GetTypeAsString($1), 'pointer']);
                                                           yyabort;
                                                        end;
                                                        $$ := TParserHelper.GetOriginalType($1);
                                                }
;

table_index:
                        statement               {
                                                        if not TParserHelper.IsIntegerType($1) then
                                                        begin
                                                           yyerrmsg := i18Manager.GetFormattedString('BadIndx', [TParserHelper.GetTypeAsString($1)]);
                                                           yyabort;
                                                        end;
							pcount := 1;
                                                }

                |       table_index ',' table_index {   Inc(pcount);    }
;

table_exp:

			var_const '[' table_index ']'	{
                                                        arg1 := TParserHelper.GetIdentInfo($1);
							if arg1.TypeOriginal <> PASCAL_STRING_TYPE then
							begin
							   if arg1.IdentType <> VARRAY then
                                                           begin
                                                              yyerrmsg := i18Manager.GetFormattedString('BadArrExp', [sLineBreak, $1]);
                                                              yyabort;
                                                           end;
							   if arg1.DimensCount <> pcount then
							   begin
							      yyerrmsg := i18Manager.GetFormattedString('BadIndxNumber', [pcount, $1, sLineBreak, arg1.DimensCount]);
							      yyabort;
							   end;
							end
							else
							begin
							   if arg1.IdentType = VARIABLE then
							   begin
							      if pcount <> 1 then
							      begin
							         yyerrmsg := i18Manager.GetFormattedString('BadIndxNumber', [pcount, $1, sLineBreak, 1]);	
							         yyabort;
							      end
							      else
							      begin
							         $$ := PASCAL_CHAR_TYPE;
							         exit;
							      end;
							   end
							   else if not (pcount in [arg1.DimensCount, arg1.DimensCount+1]) or ((pcount = arg1.DimensCount+1) and (PASCAL_CHAR_TYPE = UNKNOWN_TYPE)) then
							   begin
							      yyerrmsg := i18Manager.GetFormattedString('BadIndxNumber', [pcount, $1, sLineBreak, arg1.DimensCount]);
							      yyabort;
							   end
							   else
							   begin
							      $$ := PASCAL_CHAR_TYPE;
							      exit;
							   end;
							end;
							$$ := arg1.TypeOriginal;
						}

		|	table_exp '^'		{
                                                        if not TParserHelper.IsPointerType($1) then
                                                        begin
                                                           yyerrmsg := i18Manager.GetFormattedString('IncTypes', [TParserHelper.GetTypeAsString($1), 'pointer']);
                                                           yyabort;
                                                        end;
                                                        $$ := TParserHelper.GetOriginalType($1);
                                                }
;

output_statement:	
			statement		{
							if TParserHelper.IsPointerType($1) or TParserHelper.isRecordType($1) or ($1 = UNKNOWN_TYPE) then
                                                        begin
                                                           yyerrmsg := i18Manager.GetString('BadOutput');
                                                           yyabort;
                                                        end;
							$$ := $1;
						}


		|	output_statement ':' statement  {
							if not TParserHelper.IsIntegerType($3) then
                                                        begin
                                                           yyerrmsg := i18Manager.GetFormattedString('BadFuncParm', [TParserHelper.GetTypeAsString($3), 'Writeln']);
                                                           yyabort;
                                                        end;
							$$ := $1;
						}

		|	output_statement ',' output_statement	{
							if $3 = PASCAL_TEXT_FILE_TYPE then
                                                        begin
                                                           yyerrmsg := i18Manager.GetString('BadOutput');
                                                           yyabort;
                                                        end;
							$$ := $1;
						}	
;

range:			statement		{
							rval := TParserHelper.GetUserFunctionType;
							lIsEnum := TParserHelper.IsEnumType($1);
							lIsInteger := TParserHelper.IsIntegerType($1);
							if not is_constant and (yymode = yymVarSize) then
							begin
							   yyerrmsg := i18Manager.GetString('NotConst');
							   yyabort;
							end
							else if  (yymode = yymReturn) and (($1 <> rval) and not (TParserHelper.IsPointerType(rval) and ($1 = GENERIC_PTR_TYPE)) and not ((TParserHelper.IsNumericType(rval) and TParserHelper.IsNumericType($1)) and not (TParserHelper.IsIntegerType(rval) and TParserHelper.IsRealType($1)))) then
							begin
							   yyerrmsg := i18Manager.GetFormattedString('IncTypes', [TParserHelper.GetTypeAsString($1), TParserHelper.GetTypeAsString(rval)]);
							   yyabort;	
							end
							else if yymode in [yymFor, yymCase, yymCaseValue, yymVarSize] then
							begin	
							   if not (yymode in [yymFor, yymVarSize]) then
							   begin
							      if (yymode = yymCaseValue) and TParserHelper.IsDuplicatedCase then
                                                              begin
                                                                 yyerrmsg := i18Manager.GetString('DupCaseVal');
                                                                 yyabort;
                                                              end
							      else if ($1 <> PASCAL_CHAR_TYPE) and not lIsInteger and not lIsEnum then
                                                              begin
                                                                 yyerrmsg := i18Manager.GetFormattedString('IncTypes', [TParserHelper.GetTypeAsString($1), 'integer']);
                                                                 yyabort;
                                                              end
							      else if yymode = yymCaseValue then
							      begin
							         lType := TParserHelper.GetCaseVarType;
								 if (lType <> $1) and not (TParserHelper.IsIntegerType(lType) and lIsInteger) then
							         begin
                                                                    yyerrmsg := i18Manager.GetFormattedString('IncTypes', [TParserHelper.GetTypeAsString(lType), TParserHelper.GetTypeAsString($1)]);
                                                                    yyabort;
								 end;
							      end;
							   end
							   else if yymode = yymFor then
							   begin
  							      lType := TParserHelper.GetForVarType;
							      if (lType <> $1) and not (TParserHelper.IsIntegerType(lType) and lIsInteger) then
                                                              begin
                                                                 yyerrmsg := i18Manager.GetFormattedString('IncTypes', [TParserHelper.GetTypeAsString(lType), TParserHelper.GetTypeAsString($1)]);
                                                                 yyabort;
                                                              end;
							   end
                                                           else if not lIsInteger and (yymode = yymVarSize) then
							      yyabort;
							end;
						}

		|	range ',' statement	{	if yymode <> yymVarSize then yyabort;	}
;

case:		
			range

		|	range T_DBLDOT range

		|	case ',' case
;


err:			ILLEGAL			{ yyabort; }
;        

%%

{$I Pascal_Lexer}

end.
