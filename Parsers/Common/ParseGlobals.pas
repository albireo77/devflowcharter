unit ParseGlobals;
{ Test noddy for yacc generated parser - global variables etc.	}
{ Author C.P.Osborne.							}
{ Modified:								}
{	26/09/2000	CPO	Created.				}
interface
type
  TVState = (tUndefined, tReal, tBoolean, tString);
  TVariable = record
		State: TVState;
		SVal: String;
		case Integer of
		  0: (RVal: Real);
		  1: (BVal: Boolean);
	    end;
procedure InitialiseVariables;
var
  Parse_Reals: array [1..26] of TVariable;	{ Real variables	}
  Parse_Bools: array [1..26] of TVariable;	{ Boolean variables	}
  Parse_Strings: array [1..26] of TVariable;	{ String variables	}
  Parse_If: Boolean;
  Parse_Run: Boolean;
  Parse_Error: Integer;	{ Run-time error code.			}
				{ 0 = syntax error.			}
				{ 1 = division by zero.			}
				{ 2 = undefined variable.		}
implementation
uses
  SysUtils, Dialogs;
procedure InitialiseVariables;
var
  i: Integer;
begin
  Parse_If := True;
  Parse_Run	:= True;
  Parse_Error := 0;		{ 0 = syntax error.			}
  for i := 1 to 26 do
  begin
    Parse_Reals[i].State := tUndefined;
    Parse_Bools[i].State := tUndefined;
    Parse_Strings[i].State := tUndefined;
  end;
end;
begin
  InitialiseVariables;
end.