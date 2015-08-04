unit ParseGlobals;
{ Test noddy for yacc generated parser - global variables etc.	}
{ Author C.P.Osborne.							}
{ Modified:								}
{	26/09/2000	CPO	Created.				}

interface
type
  TVState	= (tUndefined, tReal, tBoolean, tString);

  TVariable = record
		State   : TVState;
		SVal	: String;
		case Integer of
		0: ( RVal : Real );
		1: ( BVal : Boolean );
	      end;

procedure InitialiseVariables;

var
  Parse_Reals	: array [1..26] of TVariable;	{ Real variables	}
  Parse_Bools	: array [1..26] of TVariable;	{ Boolean variables	}
  Parse_Strings	: array [1..26] of TVariable;	{ String variables	}
  Parse_If	: Boolean;
  Parse_Run	: Boolean;
  Parse_Error	: Integer;	{ Run-time error code.			}
				{ 0 = syntax error.			}
				{ 1 = division by zero.			}
				{ 2 = undefined variable.		}

implementation

uses
  SysUtils, Dialogs;

procedure InitialiseVariables;
var
  I	: Integer;
begin
  { Initialise the states & variables...				}
  Parse_If	:= True;
  Parse_Run	:= True;
  Parse_Error	:= 0;		{ 0 = syntax error.			}
  for I := 1 to 26 do
  begin
    Parse_Reals[I].State	:= tUndefined;
    Parse_Bools[I].State	:= tUndefined;
    Parse_Strings[I].State	:= tUndefined;
  end;
end;	{ InitialiseVariables	}

begin
  InitialiseVariables;
end.
