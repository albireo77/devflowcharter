@ set CURR_DIR=%CD%

@rem ------------------- generate lexer source
..\Common\lex.exe /v Pascal_Lexer.l

@rem ------------------- generate parser source
@ copy Pascal_Parser.y ..\Common
@ cd ..\Common
yacc.exe /v Pascal_Parser.y
@ copy Pascal_Parser.pas %CURR_DIR%
@ copy Pascal_Parser.lst %CURR_DIR%
@ del Pascal_Parser.pas
@ del Pascal_Parser.lst
@ del Pascal_Parser.y
@ cd %CURR_DIR%
