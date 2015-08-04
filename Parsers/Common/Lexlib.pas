unit LexLib;

{ Standard Lex library unit for TP Lex Version 3.0.2-11-91 AG		}

{ Extended by Thierry Coq, sept. 1997					}
{ adapted to Delphi 3							}
{ Notes :								}
{         - input and output files cannot be used by non-console Delphi	}
{           applications, so streams have to be used.			}
{         - the current lexlib library is not object, and therefore one	}
{           cannot load several lexers, for example.			}
{           => The lexlib interface is transformed into a Lexer object	}
{              which can then be extended by the lex program.		}

{ Modified September 2000 by C.P.Osborne for Delphi 4/5			}
{	12/09/2000	CPO	Mods started.				}
{				No longer a TComponent.			}
{				Added TLexFile items instead of streams.}
{				yytext, yyleng now properties.		}
{				Renamed constants Lex_...		}
{				State variables moved into object.	}


interface

{ The Lex library unit supplies a collection of variables and routines	}
{ needed by the lexical analyzer routine yylex and application		}
{ programs using Lex-generated lexical analyzers.			}
{ It also provides access to the input/output streams used by the	}
{ lexical analyzer and the text of the matched string, and provides	}
{ some utility functions which may be used in actions.			}
{									}
{ This `standard' version of the LexLib unit may be used to implement	}
{ lexical analyzers which read from and write to MS-DOS files (using	}
{ standard input and output, by default); or analysers reading from	}
{ streams or just string variables.					}
{ This will be suitable for many standard applications.			}
{ In order to increase usability you may also supply new data in/out	}
{ methods without needing to derive a new class.			}

{ Variables:								}
{									}
{  yytext contains the current match.					}
{  yyleng its length.							}
{  yyline contains the current input line.				}
{  yylineno and yycolno denote the current input position (line, column)}
{  (These values are often used in giving error diagnostics, but they	}
{  will only be meaningful if there is no rescanning across line ends)	}
{									}
{  yyinput, yyoutput, and yyerrorfile are used to source, send, and	}
{  complain. These are no longer traditional data files, but are 	}
{  TLexFile objects that can handle file, stream, or string data.	}

uses
  LexFile;

const
  Lex_max_matches	= 1024;
  Lex_max_rules		= 256;
  Lex_max_chars		= 1024;	{ Maximum characters to 'unget'		}

type
  TCustomLexer = class
  protected
    { *****************************************************************	}
    { State Variables:							}
    {									}
    { Some state information is maintained to keep track for calls to	}
    { yymore, yyless, reject, start and yymatch/yymark, and to		}
    { initialize state information used by the lexical analyzer.	}
    { - yystext:	contains the initial contents of the yytext	}
    {			variable; this will be the empty string, unless	}
    {			yymore is called which sets yystext to the	}
    {			current yytext.					}
    yystext	: String;
    { - yysstate:	Start state of lexical analyzer (set to 0	}
    {			during initialization, and modified in calls to	}
    {			the start routine).				}
    yysstate	: Integer;
    { - yylstate:	line state information (1 if at beginning of	}
    {			line, 0 otherwise).				}
    yylstate	: Integer;
    { - yystack:	stack containing matched rules; yymatches	}
    {			contains the number of matches.			}
    yymatches	: Integer;
    yystack	: array [1..Lex_max_matches] of Integer;
    { - yypos:		for each rule the last marked position (yymark);}
    {			zeroed when rule has already been considered.	}
    yypos	: array [1..Lex_max_rules] of Integer;
    { - yysleng:	copy of the original yyleng used to restore	}
    {			state information when reject is used.		}
    yysleng	: Integer;
    { *****************************************************************	}

    yystate	: Integer;	{ Current state of lexical analyzer.	}
    yyactchar	: Char;		{ Current character.			}
    yylastchar	: Char;		{ Last matched character (#0 if none).	}
    yyrule	: Integer;	{ Matched rule.				}
    yyreject	: Boolean;	{ Current match rejected?		}
    yydone	: Boolean;	{ yylex return value set?		}
    yyretval	: Integer;	{ yylex return value.			}
    FText	: String;	{ Matched text.				}
    FPrevChar	: Char;		{ Used to sort CR/LF.			}

    { Unget buffer...							}
    Bufptr	: Integer;
    Buf		: array [1..Lex_max_chars] of Char;

    { Get current string length						}
    function  GetYYLeng : Integer; virtual;

    { Start the next match; initialize state information of the lexical	}
    { analyzer.								}
    procedure yynew;

    { Get next character from the input stream and update yytext and	}
    { yyactchar accordingly.						}
    procedure yyscan;

    { Mark position for rule no. N					}
    procedure yymark( N : Integer );

    { Declares a match for rule number N				}
    procedure yymatch( N : Integer );

    { Find the last match and the corresponding marked position and	}
    { adjust the matched string accordingly; return:			}
    { - true if a rule has been matched, false otherwise		}
    { - n: the number of the matched rule.				}
    function  yyfind( var N : Integer ) : Boolean;

    { Execute the default action (copy character); return true unless	}
    { at end-of-file.							}
    function  yydefault : Boolean;

    { Re-initialize state information after lexical analysis has been	}
    { finished.								}
    procedure yyclear;

    { Write a fatal error message and halt program.			}
    procedure Fatal( msg : String );

  public
    yyinput	: TLexFile;	{ Input file				}
    yyoutput	: TLexFile;	{ Output file				}
    yyerrorfile	: TLexFile;	{ Destination for errors.		}
    yyline	: String;	{ Current input line.			}
    yylineno	: Integer;	{ Current input line.			}
    yycolno	: Integer;	{ Current input column.			}
    property yytext : String read FText;
    property yyleng : Integer read GetYYLeng;

    { *****************************************************************	}
    { Constructor & destructor.						}
    { *****************************************************************	}
    constructor Create; virtual;
    destructor  Destroy; override;

    { *****************************************************************	}
    { I/O routines:
    {									}
    { The following routines get_char, unget_char and put_char are used	}
    { to implement access to the input and output files. Since \n	}
    { (newline) for Lex means line end, the I/O routines have to	}
    { translate MS-DOS line ends (carriage-return/line-feed) into	}
    { newline characters and vice versa. Input is buffered to allow	}
    { rescanning text (via unput_char).					}
    {									}
    { The input buffer holds the text of the line to be scanned. When	}
    { the input buffer empties, a new line is obtained from the input	}
    { stream. Characters can be returned to the input buffer by calls	}
    { to unget_char. At end-of-file a null character is returned.	}
    {									}
    { The input routines also keep track of the input position and set	}
    { the yyline, yylineno, yycolno variables accordingly.		}
    {									}
    { Since the rest of the Lex library only depends on these three	}
    { routines (there are no direct references to the yyinput and	}
    { yyoutput files or to the input buffer), you can easily replace	}
    { get_char, unget_char and put_char by another suitable set of	}
    { routines, e.g. if you want to read from/write to memory, etc.	}
    {									}
    { However it easier to modify input using the TLexFile items	}
    { yyinput, yyoutput etc directly since these now have support for	}
    { user assigned I/O as well as string, stream, and file facilities.	}
    { *****************************************************************	}

    { Obtain one character from the input file (null character at	}
    { end-of-file)							}
    function  Get_char : Char;

    { Return one character to the input file to be reread in subsequent	}
    { calls to Get_char.						}
    procedure Unget_char( C : Char );

    { Write one character to the output file.				}
    procedure Put_char( C : Char );

    { *****************************************************************	}
    { Utility routines:							}
    { *****************************************************************	}

    { Echo the current match to the output stream.			}
    procedure Echo;

    { Append the next match to the current one.				}
    procedure yymore;

    { Truncate yytext to size n and return the remaining characters to	}
    { the input stream.							}
    procedure yyless( N : Integer );

    { Reject the current match and execute the next one.		}
    { N.B. Reject does not actually cause the input to be rescanned; 	}
    { instead, internal state information is used to find the next	}
    { match. Hence you should not try to modify the input stream or the	}
    { yytext variable when rejecting a match.				}
    procedure Reject;

    { Set the return value of yylex.					}
    procedure Return( N : Integer );
    procedure Returnc( C : Char );

    { Put the lexical analyzer in the given start state.		}
    { state=0 denotes the default start state, other values are user	}
    { defined.								}
    procedure Start( state : Integer );

    { The yywrap function is called by yylex at end-of-file (unless you	}
    { have specified a rule matching end-of-file). You may redefine this}
    { routine in your Lex program to do application-dependent processing}
    { at end of file. In particular, yywrap may arrange for more input	}
    { and return false in which case the yylex routine resumes lexical	}
    { analysis.								}
    { The default yywrap routine supplied here closes input and output	}
    { files and returns true (causing yylex to terminate).		}
    function  yywrap : Boolean;

    { reset the lexer for another run.					}
    procedure Reset;

  end;	{ TCustomLexer	}

implementation

{ Get current string length						}
function  TCustomLexer.GetYYLeng : Integer;
begin
  Result := Length( FText );
end;	{ GetYYLeng	}

{ Write a fatal error message and halt program.				}
procedure TCustomLexer.Fatal ( Msg : String );
begin
{$Ifdef HaltOnError}
  yyerrorfile.Writeln( 'LexLib: ' +  Msg );
  Halt(1);
{$Else  HaltOnError}
  raise ELFException.Create( 'LexLib: ' + Msg );
{$Endif HaltOnError}
end;	{ Fatal	}

{ Obtain one character from the input file (null character at EOF)	}
function TCustomLexer.Get_char : Char;
var
  C	: Char;
label
  retry;
begin
  { Is there anything in the undo buffer?				}
  if Bufptr > 0 then
  begin
    Get_char := Buf[ Bufptr ];
    Dec( Bufptr );
    Inc( yycolno );
    Exit;
  end;

  { Otherwise get a character from the input file.			}
retry:
  C := yyinput.Get;
  Inc( yycolno );
  if C in [#10,#13] then
  begin
    { It's a newline, but may be part of a pair.			}
    if ( (C = #13) and (FPrevChar = #10) )
      or ( (C = #10) and (FPrevChar = #13) ) then
    begin
      { Clear the pair check & retry.					}
      FPrevChar := #0;
      goto retry;
    end;
    FPrevChar := C;
    C := #10;		{ Always use newline not CR for the analysis.	}
    Inc( yylineno );	{ We are now on next line.			}
    yycolno := 1;
  end else FPrevChar := #0;
  Get_char := C;
end;	{ Get_char	}

{ Return one character to the input file to be reread in subsequent	}
{ calls to Get_char.							}
procedure TCustomLexer.Unget_char( C : Char );
begin
  if Bufptr = Lex_max_chars then Fatal( 'input buffer overflow' );
  Inc(Bufptr);
  Dec(yycolno);
  Buf[Bufptr] := C;
end;	{ Unget_char	}

{ Write one character to the output file.				}
procedure TCustomLexer.Put_char( C : Char );
begin
  if C = #0 then
  else if c = #10 then yyoutput.Writeln( '' )
  else yyoutput.Write( C )
end;	{ Put_char	}

{ Echo the current match to the output stream.				}
procedure TCustomLexer.Echo;
var
  I	: Integer;
begin
  for I := 1 to Length(Ftext) do Put_char( Ftext[I] );
end;	{ Echo	}

{ Append the next match to the current one.				}
procedure TCustomLexer.yymore;
begin
  yystext := Ftext;
end;	{ yymore	}

{ Truncate yytext to size n and return the remaining characters to	}
{ the input stream.							}
procedure TCustomLexer.yyless( N : Integer );
var
  I	: Integer;
begin
  for I := Length(Ftext) downto N + 1 do unget_char( Ftext[I] );
  SetLength( Ftext, N );
end;	{ yyless	}

{ Reject the current match and execute the next one.			}
{ N.B. Reject does not actually cause the input to be rescanned; 	}
{ instead, internal state information is used to find the next	match.	}
{ Hence you should not try to modify the input stream or the yytext	}
{ variable when rejecting a match.					}
procedure TCustomLexer.Reject;
var
  I	: Integer;
begin
  yyreject := True;
  for I := Length( Ftext ) + 1 to yysleng do Ftext := Ftext + get_char;
  Dec( yymatches );
end;	{ Reject	}

{ Set the return value of yylex.					}
procedure TCustomLexer.Return( N : Integer );
begin
  yyretval := N;
  yydone   := True;
end;	{ Return	}

{ Set the return value of yylex.					}
procedure TCustomLexer.Returnc( C : Char );
begin
    yyretval := Ord( C );
    yydone   := True;
end;	{ Returnc	}

{ Put the lexical analyzer in the given start state.			}
{ state=0 denotes the default start state, other values are user	}
{ defined.								}
procedure TCustomLexer.Start( State : Integer );
begin
  yysstate := State;
end;	{ Start	}

{ The yywrap function is called by yylex at end-of-file (unless you	}
{ have specified a rule matching end-of-file). You may redefine this	}
{ routine in your Lex program to do application-dependent processing	}
{ at end of file. In particular, yywrap may arrange for more input	}
{ and return false in which case the yylex routine resumes lexical	}
{ analysis.								}
{ The default yywrap routine supplied here just returns true.		}
{ (causing yylex to terminate).						}
function TCustomLexer.yywrap : Boolean;
begin
  yywrap := True;
end;	{ yywrap	}

{ *********************************************************************	}
{ Internal routines:							}
{ *********************************************************************	}

{ Start the next match; initialize state information of the lexical	}
{ analyzer.								}
procedure TCustomLexer.yynew;
begin
  { set EOL state unless we have finished.				}
  if yylastchar <> #0 then
    if yylastchar = #10 then
      yylstate := 1
    else
      yylstate := 0;

  yystate	:= yysstate + yylstate;	{ saved state & EOL indicator	}
  Ftext		:= yystext;		{ restore saved match		}
  yystext	:= '';			{ no saved match now		}
  yymatches	:= 0;
  yydone	:= False;
end;	{ yynew	}

{ Get next character from the input stream and update yytext and	}
{ yyactchar accordingly.						}
procedure TCustomLexer.yyscan;
begin
  yyactchar	:= Get_char;
  Ftext		:= Ftext + yyactchar;
end;	{ yyscan	}

{ Mark position for rule no. N						}
procedure TCustomLexer.yymark( N : Integer );
begin
  if N > Lex_max_rules then Fatal('too many rules');
  yypos[N] := Length(Ftext);
end;	{ yymark	}

{ Declare a match for rule number N					}
procedure TCustomLexer.yymatch( N : Integer );
begin
  Inc( yymatches );
  if yymatches > Lex_max_matches then Fatal('match stack overflow');
  yystack[ yymatches ] := N;
end;	{ yymatch	}

{ Find the last match and the corresponding marked position and		}
{ adjust the matched string accordingly. Return:			}
{ - true if a rule has been matched, false otherwise			}
{ - n: the number of the matched rule.					}
function TCustomLexer.yyfind( var N : Integer ) : Boolean;
begin
  yyreject := False;
  while ( yymatches > 0 ) and ( yypos[ yystack[ yymatches ] ] = 0 ) do
    Dec( yymatches );

  if yymatches > 0 then
  begin
    yysleng	:= Length(Ftext);
    N		:= yystack[ yymatches ];
    yyless( yypos[N] );
    yypos[N]	:= 0;
    if Length( Ftext ) > 0 then yylastchar := Ftext[Length(Ftext)]
      else yylastchar := #0;
    yyfind	:= true;
  end else
  begin
    yyless( 0 );
    yylastchar	:= #0;
    yyfind	:= False;
  end
end;	{ yyfind	}

{ Execute the default action (copy character). Return true unless	}
{ at end-of-file.							}
function TCustomLexer.yydefault : Boolean;
begin
  yyreject	:= False;
  yyactchar	:= Get_char;
  if yyactchar <> #0 then
  begin
    Put_char(yyactchar);
    yydefault	:= True;
  end else
  begin
    yylstate	:= 1;
    yydefault	:= False;
  end;
  yylastchar	:= yyactchar;
end;	{ yydefault	}

{ Re-initialize state information after lexical analysis has been	}
{ finished.								}
procedure TCustomLexer.yyclear;
begin
  Bufptr	:= 0;
  yysstate	:= 0;
  yylstate	:= 1;
  yylastchar	:= #0;
  Ftext		:= '';
  yystext	:= '';
  FPrevChar	:= #0;
end;	{ yyclear	}

{ Constructor...							}
constructor TCustomLexer.Create;
begin
  inherited create;

  { Create (closed) file objects.					}
  yyinput	:= TLexFile.Create;
  yyoutput	:= TLexFile.Create;
  yyerrorfile	:= TLexFile.Create;

  { The error file ismainly used for debug - allocate to bit bucket	}
  { for now & let user re-assign later if required.			}
  yyerrorfile.AssignBitBucket;

  { Reset to starting state.						}
  Reset;
end;	{ Create	}

{ Destructor...								}
destructor TCustomLexer.Destroy;
begin
  { Clean up file objects						}
  yyinput.Free;
  yyoutput.Free;
  yyerrorfile.Free;
  inherited Destroy;
end;	{ Destroy	}

{ Reset the Lexer for another go.					}
procedure TCustomLexer.Reset;
begin
  yylineno	:= 1;
  yycolno	:= 0;
  yyclear;
end;	{ Reset	}

end.
