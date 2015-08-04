
unit YaccLib;

{ Yacc Library Unit for TP Yacc Version 3.0, 6-17-91 AG 		}
{ adapted to Delphi 3, 20/9/97 						}

{ Modified September 2000 by C.P.Osborne for Delphi 4/5			}
{	11/09/2000	CPO	Mods started.				}
{				Dropped YaccUtilities.			}
{				No longer based on component.		}

interface

uses Classes, LexLib;

const yymaxdepth = 2048;	{ default stack size of parser.		}

type
  { default value type, may be redefined in Yacc output file.		}
  YYSType = Integer;
  TYYFlag = ( yyfnone, yyfaccept, yyfabort, yyferror );

  TCustomParser = Class
  protected
    { Flags used internally by parse routine.				}
    yyerrflag	: Integer;
    yyflag	: TYYFlag;
  public
    ylex	: TCustomLexer;	{ Our lexical analyser.			}
    yychar	: Integer; 	{ Current lookahead character.		}
    yynerrs	: Integer; 	{ Current number of syntax errors as	}
				{ reported by the parser.		}
    yydebug	: Boolean;	{ Set to true to enable debugging	}
				{ output from parser.			}
    yydebuglex	: Boolean;	{ Set to true to echo all lex tokens to	}
				{ the debug channel.			}

    { Display an error message.						}
    procedure yyerror( msg : String );

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

    { Write a text linne to the debug/error channel.			}
    procedure EWriteln( S : String );
  end;	{ TCustomParser }

implementation

{ We pass error calls to the yyerrorfile item in the lexer.		}
{ This requires the TLexFile item to have been opened at some stage,	}
{ as there is no longer a default error channel (programs are windowed)	}
procedure TCustomParser.yyerror( msg : String );
begin
  ylex.yyerrorfile.Writeln( msg );
end;	{ yyerrmsg      }

{ Delete current lookahead token.					}
procedure TCustomParser.yyclearin;
begin
  yychar := -1;
end;	{ yyclearin	}

{ Trigger accept action of parser (yyparse will return 0)		}
procedure TCustomParser.yyaccept;
begin
  yyflag := yyfaccept;
end;	{ yyaccept	}

{ Trigger abort action (yyparse will return 1)				}
procedure TCustomParser.yyabort;
begin
  yyflag := yyfabort;
end;	{ yyabort	}

{ Force error recovery as if syntax error had been located.		}
procedure TCustomParser.yyerrlab;
begin
  yyflag := yyferror;
end;	{ yyerrlab	}

{ Reset parser to normal operation after an error.			}
procedure TCustomParser.yyerrok;
begin
  yyerrflag := 0;
end;	{ yyerrok	}

{ Write a text linne to the debug/error channel.			}
procedure TCustomParser.EWriteln(S: String);
begin
  ylex.yyerrorfile.Writeln( S );
end;	{ EWriteln	}

end.
