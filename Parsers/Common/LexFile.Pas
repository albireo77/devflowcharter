unit LexFile;
{ Lex file type for use by the Lexical analyser library.		}
{ This enables input/output from/to files, streams & strings etc.	}
{ Author C.P.Osborne.	}
{ Modified: }
{	12/09/2000	CPO	Created.}
{ 21/01/2011 Michal Domagala. }
{            Modifications for devFlowcharter: remove unnecessary stuff. }

interface

uses
  System.SysUtils;

type
  ELFException = class(Exception);
  TDir = (ddIn, ddOut, ddBoth);

  { I/O can be by a user supplied routine or method. In this case }
  { a single call is used to put or get a character, or check for EOF. }
  { For EOF the function should return #0 if EOF, else any other char. }
  TFmode =	(tfGet, tfPut, tfEOF, tfClose);
  TLexFile = class
		protected
		  IO_String: AnsiString;
		  FDir: TDir;
		  FLength: Integer;	{ String length	}
		  FPos: Integer;	{ String position }
		public
		  property Direction: TDir read FDir;

		  { Set string & force to string input type. }
		  procedure AssignString(const S: AnsiString); virtual;

		  { Set into bit-bucket mode. }
		  procedure AssignBitBucket;

		  { Send a character }
		  procedure Put(C: AnsiChar); virtual;

		  { Send a lot of text.	}
		  procedure Write(const S: AnsiString); virtual;

		  { Send a lot of text & a new line. }
		  procedure Writeln(const S: AnsiString); virtual;

		  { Get a character }
		  function Get: AnsiChar; virtual;

		  { Are we at EOF? }
		  function EOF: Boolean; virtual;

		  { Close the file or whatever. }
		  procedure Close;
		end;

implementation

{$I+}		{ Raise exceptions on normal I/O errors. }

{ Make I/O into a bit bucket. }
procedure TLexFile.AssignBitBucket;
begin
  Close;
  FDir := ddBoth;
end;

{ Set string & force to string input type. }
procedure TLexFile.AssignString(const S: AnsiString);
begin
  Close;
  IO_String := S;
  FLength := Length(S);
  FPos := 0;
  FDir := ddIn;
end;

{ Close the file or whatever. }
procedure TLexFile.Close;
begin
  IO_String := '';
end;

{ Are we at EOF? }
function TLexFile.EOF: Boolean;
begin
  result := FPos = FLength;
end;

{ Get a character }
function TLexFile.Get: AnsiChar;
begin
  if FDir = ddOut then
    raise ELFException.Create('Not an input file');
  if EOF then
    result := #0
  else
  begin
    Inc(FPos);
    result := IO_String[FPos];
  end;
end;

{ Send a lot of text. }
procedure TLexFile.Write(const S: AnsiString);
begin
  if FDir = ddIn then
    raise ELFException.Create('Not an output file');
  IO_String := IO_String + S;
  FPos := Length(IO_String);
  FLength := FPos;
end;

{ Send a lot of text & a new line. }
procedure TLexFile.Writeln(const S: AnsiString);
begin
  Write(sLineBreak + S);
end;

{ Send a character. }
procedure TLexFile.Put(C: AnsiChar);
begin
  Write(C);
end;

end.
