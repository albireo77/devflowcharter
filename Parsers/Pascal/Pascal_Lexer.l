L				[A-Za-z]
D				[0-9]

%%
[ ]       		        ; // ignore spaces

[Ss][Ii][Nn]		return(T_SIN);

[Cc][Oo][Ss]		return(T_COS);

[Tt][Aa][Nn]		return(T_TAN);

[Cc][Oo][Tt][Aa][Nn]	return(T_COTAN);

[Ll][Nn]		return(T_LN);

[Ee][Xx][Pp]		return(T_EXP);

[Ss][Qq][Rr]		return(T_SQR); 	

[Rr][Aa][Nn][Dd][Oo][Mm]	return(T_RAND);

[Tt][Rr][Uu][Nn][Cc]	return(T_TRUNC);

[Ww][Rr][Ii][Tt][Ee][Ll][Nn]	return(T_WRITELN);
                              
[Rr][Ee][Aa][Dd][Ll][Nn]	return(T_READLN);

[Ww][Rr][Ii][Tt][Ee]	return(T_WRITE);

[Rr][Ee][Aa][Dd]	return(T_READ);

[Nn][Oo][Tt]		return(T_NOT);

[Oo][Rr]		return(T_OR);

[Aa][Nn][Dd]		return(T_AND);

[Aa][Bb][Ss]		return(T_ABS);

[Ll][Ee][Nn][Gg][Tt][Hh]	return(T_LEN);

[Ss][Qq][Rr][Tt]	return(T_SQRT);

[Dd][Ii][Vv]		return(T_DIV);

[Oo][Rr][Dd]		return(T_ORD);

[Cc][Hh][Rr]		return(T_CHR);

[Mm][Oo][Dd]		return(T_MOD);

[Bb][Rr][Ee][Aa][Kk]	return(T_BREAK);

[Cc][Oo][Nn][Tt][Ii][Nn][Uu][Ee]	return(T_CONTINUE);

[Ee][Xx][Ii][Tt]	return(T_EXIT);

[Aa][Ss][Ss][Ii][Gg][Nn]	return(T_ASSIGN);

[Rr][Ee][Ss][Ee][Tt]	return(T_RESET);

[Rr][Ee][Ww][Rr][Ii][Tt][Ee]	return(T_REWRITE);

[Aa][Pp][Pp][Ee][Nn][Dd]	return(T_APPEND);

[Cc][Ll][Oo][Ss][Ee]	return(T_CLOSE);

[Ee][Oo][Ff]		return(T_EOF);

[Ee][Oo][Ll][Nn]	return(T_EOLN);

[Pp][Ii]		begin
				yylval.yyString := Copy(yytext, 1, Length(yytext));
				return(T_PI);
			end;

[Dd][Ii][Ss][Pp][Oo][Ss][Ee]	return(T_DISPOSE);

[Nn][Ee][Ww]		return(T_NEW);

[Nn][Ii][Ll]		return(T_NIL);

[Tt][Rr][Uu][Ee]	return(L_BOOL);

[Ff][Aa][Ll][Ss][Ee]	return(L_BOOL);

{D}+			begin
				yylval.yyInteger := StrToInt(yytext);
				return(L_INT);
			end;

{D}+(\.{D}+)?([Ee][+-]?{D}+)?	return(L_REAL);

[$][0-9A-Fa-f]+		return(L_HEX);

[_A-Za-z][A-Za-z0-9_]*	begin
				yylval.yyString := Copy(yytext, 1, Length(yytext));
				return(T_IDENTIFIER);
			end;
                                    
['][^\']*[']		begin                             
				yylval.yyString := Copy(yytext, 1, Length(yytext));
				return(T_STRING);
			end;

[-+*/=\^\(\)\[\],:;@\.]	returnc(yytext[1]);
                           
":="			return(T_ASSIGN_SIGN);

".."			return(T_DBLDOT);
                                                               
[<>]			return(GL);
                                                             
>=			return(GE);
                                                            
\<=			return(LE);
                              
\<>			return(NE);
                              
.			return(ILLEGAL);
                             