unit LexFile;
{ Lex file type for use by the Lexical analyser library.		}
{ This enables input/output from/to files, streams & strings etc.	}
{ Author C.P.Osborne.							}
{ Modified:								}
{	12/09/2000	CPO	Created.				}
{ 21/01/2011 Michal Domagala. }
{            Modifications for devFlowcharter: comment out unnecessary stuff. }

interface

uses
  Classes, SysUtils;

const
  LexEOL	: String = #13#10;	{ Allow user to change this!	}

type
  ELFException	= class( Exception );
  TLexType =	({ttText, ttStream,} ttString, {ttFunction, ttMethod,} ttNull, ttClosed);
  TDir	   =	(ddIn, ddOut, ddBoth);

  { I/O can be by a user supplied routine or method. In this case a	}
  { single call is used to put or get a character, or check for EOF.	}
  { For EOF the function should return #0 if EOF, else any other char.	}
  TFmode   =	(tfGet, tfPut, tfEOF, tfClose);
  //TLexUsrFn=	function( mode : TFmode; C : Char ) : Char;
  //TLexUsrMd=	function( mode : TFmode; C : Char ) : Char of Object;
  TLexFile =	class
		protected
		  //IO_File	: Text;
		  IO_String	: String;
		  //IO_Stream	: TStream;
		  //IO_Function	: TLexUsrFn;
		  //IO_Method	: TLexUsrMd;
		  FType		: TLexType;
		  FDir		: TDir;
		  FLength	: Integer;	{ String length		}
		  FPos		: Integer;	{ String position	}
		public
		  property Direction : TDir read FDir;
		  property FileType  : TLexType read FType;

		  { Get string result if output string type.		}
		  //function GetString : String; virtual;

		  { Set string & force to string input type.		}
		  procedure AssignString(const S : String ); virtual;

		  { Set to string output type.				}
		  //procedure AssignStringOut; virtual;

		  { Set file for output.				}
		  //procedure AssignOutput( Name : String ); virtual;

		  { Set file for input.					}
		  //procedure AssignInput( Name : String ); virtual;

		  { Set Stream for I/O					}
		  //procedure AssignStream( S : TStream; Output : Boolean );

		  { Assign I/O function.				}
		  //procedure AssignFunction( F : TLexUsrFn );

		  { Assign I/O method.					}
		  //procedure AssignMethod( M : TLexUsrMd );

		  { Set into bit-bucket mode.				}
		  procedure AssignBitBucket;

		  { Send a character (if output!)			}
		  procedure Put( C : Char ); virtual;

		  { Send a lot of text.					}
		  procedure Write( S : String ); virtual;

		  { Send a lot of text & a new line.			}
		  procedure Writeln( S : String ); virtual;

		  { Get a character (if input!)				}
		  function  Get : Char; virtual;

		  { Read a whole line of text				}
		  //function  Readln : String; virtual;

		  { Are we at EOF?					}
		  function EOF : Boolean; virtual;

		  { Close the file or whatever.				}
		  procedure Close;

		  { Constructor creates file in closed state.		}
		  Constructor Create;

		  { Destructor must clean up files etc.			}
		  Destructor  Destroy; override;
		end;	{ TLexFile	}

implementation

{$I+}		{ Raise exceptions on normal I/O errors.		}

{ *********************************************************************	}
{ TLexFile								}
{ *********************************************************************	}

{ Make I/O into a bit bucket.						}
procedure TLexFile.AssignBitBucket;
begin
  Close;
  FDir  := ddBoth;
  FType := ttNull;
end;	{ AssignBitBucket	}

{ Assign a function for I/O.						}
{procedure TLexFile.AssignFunction(F: TLexUsrFn);
begin
  Close;
  IO_Function := F;
  FDir  := ddBoth;
  FType := ttFunction;
end;}	{ AssignFunction	}

{ Set file for input.							}
{procedure TLexFile.AssignInput(Name: String);
begin
  Close;
  AssignFile( IO_File, Name );
  Reset( IO_File );
  FDir  := ddIn;
  FType := ttText;
end;}	{ AssignInput	}

{ Set a method for I/O							}
{procedure TLexFile.AssignMethod(M: TLexUsrMd);
begin
  Close;
  IO_Method := M;
  FDir  := ddBoth;
  FType := ttMethod;
end;}	{ AssignMethod	}

{ Set file for output.							}
{procedure TLexFile.AssignOutput(Name: String);
begin
  Close;
  AssignFile( IO_File, Name );
  Rewrite( IO_File );
  FDir  := ddOut;
  FType := ttText;
end;}	{ AssignOutput	}

{ Set Stream for I/O							}
{ Note: The TLexfile will take ownership of the stream & free it when	}
{ closed!								}
{procedure TLexFile.AssignStream(S: TStream; Output: Boolean);
begin
  Close;
  IO_Stream := S;
  if OutPut then FDir := ddOut else FDir := ddIn;
  FType := ttStream;
end;}	{ AssignStream	}

{ Set string & force to string input type.				}
procedure TLexFile.AssignString(const S: String);
begin
  Close;
  IO_String	:= S;
  FLength	:= Length( S );
  FPos		:= 0;
  FDir		:= ddIn;
  FType		:= ttString;
end;	{ AssignString	}

{ Set to string output type.						}
{procedure TLexFile.AssignStringOut;
begin
  Close;
  IO_String	:= '';
  FLength	:= 0;
  FPos		:= 0;
  FDir		:= ddOut;
  FType		:= ttString;
end;}	{ AssignStringOut	}

{ Close the file or whatever.						}
procedure TLexFile.Close;
begin
  case FType of
  //ttText  	: CloseFile( IO_File );
  ttString	: IO_String := '';
  {ttStream	: begin
		    IO_Stream.Free;
		    IO_Stream := nil;
		  end;}
  //ttFunction	: IO_Function( tfClose, #0 );
  //ttMethod	: IO_Method( tfClose, #0 );
  end;
  FType := ttClosed;
end;	{ Close	}

{ Constructor just marks the file as closed.				}
constructor TLexFile.Create;
begin
  FType := ttClosed;
end;	{ Create	}

{ Destructor must clean up files etc.					}
destructor TLexFile.Destroy;
begin
  { Clean up junk by closing.						}
  Close;
  inherited;
end;	{ Destroy	}

{ Are we at EOF?							}
function TLexFile.EOF: Boolean;
begin
  case FType of
  //ttText  	: EOF := System.EOF( IO_File );
  ttString	: EOF := FPos = FLength;
  //ttStream	: EOF := IO_Stream.position >= IO_Stream.size;
  //ttFunction	: EOF := IO_Function( tfEOF, #0 ) = #0;
  //ttMethod	: EOF := IO_Method( tfEOF, #0 ) = #0;
  else EOF := True;
  end;
end;	{ EOF	}

{ Get a character (if input!)						}
function TLexFile.Get: Char;
//var
//  C	: Char;
begin
  if Ftype = ttClosed then raise ELFException.Create('File is closed');
  if FDir  = ddOut then raise ELFException.Create('Not an input file');
  if EOF then Get := #0
  else case FType of
  {ttText	: begin
		    System.Read( IO_File, C );
		    Get := C;
		  end;}
  ttString	: begin
		    Inc( FPos );
		    Get := IO_String[ FPos ];
		  end;
  {ttStream	: begin
		    IO_Stream.Read( C, 1 );
		    Get := C;
		  end;}
  //ttFunction	: Get := IO_Function( tfGet, #0 );
  //ttMethod	: Get := IO_Method( tfGet, #0 );
  else Get := #0;
  end;
end;	{ Get	}

{ Get string result if output string type.				}
{function TLexFile.GetString: String;
begin
  if ( FType <> ttString ) or ( FDir = ddIn ) then
    raise ELFException.Create('Not an output string file');
  GetString := IO_String;
end;}	{ GetString	}

{ Send a character (if output!)						}
procedure TLexFile.Put(C: Char);
begin
  if Ftype = ttClosed then raise ELFException.Create('File is closed');
  if FDir  = ddIn then raise ELFException.Create('Not an output file');
  case FType of
  //ttText  	: System.Write( IO_File, C );
  ttString	: begin
		    IO_String := IO_String + C;
		    Inc( FPos );
		    FLength := FPos;
		  end;
  //ttStream	: IO_Stream.Write( C, 1 );
  //ttFunction	: IO_Function( tfPut, C );
  //ttMethod	: IO_Method( tfPut, C );
  end;
end;	{ Put	}

{ Read a whole line of text						}
{function TLexFile.Readln: String;
var
  S	: String;
  C	: Char;
begin
  if Ftype = ttClosed then raise ELFException.Create('File is closed');
  if FDir  = ddOut then raise ELFException.Create('Not an input file');
  if EOF then Readln := ''
  else case FType of
  ttText  : begin
	      System.Readln( IO_File, S );
	      Readln := S;
	    end;
  ttString: begin
	      Inc( FPos );
	      Readln := Copy( IO_String, FPos, Flength );
	      FPos := FLength;
	    end;
  else      begin
	      S := '';
	      C := Get;
	      while not (C in [#0, #10, #12]) do
	      begin
		S := S + C;
		C := Get;
	      end;
	      Readln := S;
	    end;
  end;
end;}	{ Readln	}

{ Send a lot of text.							}
procedure TLexFile.Write(S: String);
var
  I	: Integer;
begin
  if Ftype = ttClosed then raise ELFException.Create('File is closed');
  if FDir  = ddIn then raise ELFException.Create('Not an output file');
  case FType of
  {ttText  	: begin
		    System.Write( IO_File, S );
		  end;}
  ttString	: begin
		    IO_String := IO_String + S;
		    FPos    := Length( IO_String );
		    FLength := FPos;
		  end;
  //ttStream	: IO_Stream.Write( S, Length(S) );
  ttNull	: ;
  else            begin
		    for I := 1 to Length( S ) do Put( S[I] );
		  end;
  end;
end;	{ Write	}

{ Send a lot of text & a new line.					}
procedure TLexFile.Writeln(S: String);
begin
  if Ftype = ttClosed then raise ELFException.Create('File is closed');
  if FDir  = ddIn then raise ELFException.Create('Not an output file');
  case FType of
  {ttText  	: begin
		    System.Writeln( IO_File, S );
		  end;}
  ttString	: begin
		    IO_String := IO_String + LexEOL + S;
		    FPos    := Length( IO_String );
		    FLength := FPos;
		  end;
  {ttStream	: begin
		    IO_Stream.Write( S, Length( S ) );
		    IO_Stream.Write( LexEOL, SizeOf(LexEOL) );
		  end;}
  ttNull	: ;
  else		  begin
		    Write( S );
		    Write( LexEOL );
		  end;
  end;
end;	{ Writeln	}

end.
