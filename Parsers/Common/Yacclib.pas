

unit YaccLib;

{ Yacc Library Unit for TP Yacc Version 3.0, 6-17-91 AG 		}
{ adapted to Delphi 3, 20/9/97 						}
{ Modified September 2000 by C.P.Osborne for Delphi 4/5			}
{	11/09/2000	CPO	Mods started.				}
{				Dropped YaccUtilities.			}
{				No longer based on component.		}

interface

uses
   LexLib;

type
  { default value type, may be redefined in Yacc output file.		}
  YYSType = Integer;
  TYYFlag = (yyfnone, yyfaccept, yyfabort, yyferror);
  TYYMode = (yymUndefined, yymCondition, yymAssign, yymInput, yymOutput, yymFor, yymFuncCall, yymCase, yymCaseValue, yymReturn, yymVarSize);
  TYYModeSet = set of TYYMode;

  TCustomParser = Class
  protected
    { Flags used internally by parse routine.				}
    yyerrflag: Integer;
    yyflag: TYYFlag;
    yyerrmsg: String;
    procedure CheckMode(AValidModes: TYYModeSet);
  public
    ylex: TCustomLexer;	{ Our lexical analyser.			}
    yychar: Integer; 	{ Current lookahead character.		}
    yynerrs: Integer; 	{ Current number of syntax errors as reported by the parser.	}
{$IFDEF DEBUG}
    yydebug: Boolean;	{ Set to true to enable debugging output from parser. }
    yydebuglex: Boolean;	{ Set to true to echo all lex tokens to	the debug channel. }
{$ENDIF}
    yymode: TYYMode;
    { Display an error message.						}
    procedure yyerror(const msg: AnsiString);
    { Delete current lookahead token.					}
    procedure yyclearin;
    { Trigger accept action of parser (yyparse will return 0)		}
    procedure yyaccept;
    { Trigger abort action (yyparse will return 1)			}
    procedure yyabort;
    { Force error recovery as if syntax error had been located.		}
    procedure yyerrlab;
    { Reset parser to normal operation after an error.			}
    procedure yyerrok;
{$IFDEF DEBUG}
    { Write a text linne to the debug/error channel.			}
    procedure EWriteln(const S: AnsiString);
{$ENDIF}
    { Reset the parser for another run.					}
    procedure Reset;
    { Return error message.					}
    function GetErrMsg: String; 
  end;
  
const
   yymaxdepth = 2048;	{ default stack size of parser.		}
   PARSER_ERROR_KEYS: array[TYYMode] of string = ('BadGeneric', 'BadCondition', 'BadAssign', 'BadInput', 'BadOutput',
                                                  'BadFor', 'BadFunction', 'BadCase', 'BadCase', 'BadReturnVal', '');

implementation

uses
   Infrastructure;

{ We pass error calls to the yyerrorfile item in the lexer.		}
{ This requires the TLexFile item to have been opened at some stage,	}
{ as there is no longer a default error channel (programs are windowed)	}

procedure TCustomParser.yyerror(const msg: AnsiString);
begin
  ylex.yyerrorfile.Writeln(msg);
end;

{ Delete current lookahead token.					}
procedure TCustomParser.yyclearin;
begin
  yychar := -1;
end;

{ Trigger accept action of parser (yyparse will return 0)		}
procedure TCustomParser.yyaccept;
begin
  yyflag := yyfaccept;
end;

{ Trigger abort action (yyparse will return 1)				}
procedure TCustomParser.yyabort;
begin
  yyflag := yyfabort;
end;

{ Force error recovery as if syntax error had been located.		}
procedure TCustomParser.yyerrlab;
begin
  yyflag := yyferror;
end;

{ Reset parser to normal operation after an error.			}
procedure TCustomParser.yyerrok;
begin
  yyerrflag := 0;
end;

{$IFDEF DEBUG}
{ Write a text linne to the debug/error channel.			}
procedure TCustomParser.EWriteln(const S: AnsiString);
begin
  ylex.yyerrorfile.Writeln(S);
end;
{$ENDIF}

procedure TCustomParser.Reset;
begin
  yymode := yymUndefined;
  yyerrmsg := '';
end;

procedure TCustomParser.CheckMode(AValidModes: TYYModeSet);
begin
  if not (yymode in AValidModes) then
  begin
    yyerrmsg := i18Manager.GetString(PARSER_ERROR_KEYS[yymode]);
    yyabort;
  end;  
end;

function TCustomParser.GetErrMsg: String;
begin
  if yyerrmsg <> '' then
    result := yyerrmsg
  else if yyflag <> yyfaccept then
    result := i18Manager.GetString(PARSER_ERROR_KEYS[yymode])
  else
    result := '';  
end;

end.
