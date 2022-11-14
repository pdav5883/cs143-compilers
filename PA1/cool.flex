/*
 *  The scanner definition for COOL.
 */

/*
 *  Stuff enclosed in %{ %} in the first section is copied verbatim to the
 *  output, so headers and global definitions are placed here to be visible
 * to the code in the file.  Don't remove anything that was here initially
 */
%{
#include <cool-parse.h>
#include <stringtab.h>
#include <utilities.h>

/* The compiler assumes these identifiers. */
#define yylval cool_yylval
#define yylex  cool_yylex

/* Max size of string constants */
#define MAX_STR_CONST 1025
#define YY_NO_UNPUT   /* keep g++ happy */

extern FILE *fin; /* we read from this file */

/* define YY_INPUT so we read from the FILE fin:
 * This change makes it possible to use this scanner in
 * the Cool compiler.
 */
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
	if ( (result = fread( (char*)buf, sizeof(char), max_size, fin)) < 0) \
		YY_FATAL_ERROR( "read() in flex scanner failed");

char string_buf[MAX_STR_CONST]; /* to assemble string constants */
char *string_buf_ptr;

extern int curr_lineno;
extern int verbose_flag;

extern YYSTYPE cool_yylval;

/*
 *  Add Your own definitions here
 */
int mylen;
int comment_depth = 0;

%}

/*
 * Start states
 */
%x comment
%x string
/*
 * Define names for regular expressions here.
 */

DIGIT		[0-9]
LOWER		[a-z]
UPPER		[A-Z]
IDCHAR		[_A-Za-z0-9]
LINE		\n
WHITE           [ \f\r\t\v]+
%%

 /*
  *  Basics
  */
{WHITE}			{ }
{LINE}			{ curr_lineno++; }
{DIGIT}+		{ cool_yylval.symbol = inttable.add_string(yytext);
			  return INT_CONST; }

 /*
  * Keywords are case-insensitive except for the values true and false,
  * which must begin with a lower-case letter.
  */

(?i:class)		{ return CLASS; }
(?i:else)		{ return ELSE; }
(?i:fi)			{ return FI; }
(?i:if)			{ return IF; }
(?i:in)			{ return IN; }
(?i:inherits)		{ return INHERITS; }
(?i:let)		{ return LET; }
(?i:loop)		{ return LOOP; }
(?i:pool)		{ return POOL; }
(?i:then)		{ return THEN; }
(?i:while)		{ return WHILE; }
(?i:case)		{ return CASE; }
(?i:esac)		{ return ESAC; }
(?i:of) 		{ return OF; }
(?i:new)		{ return NEW; }
(?i:isvoid)		{ return ISVOID; }
(?i:not)		{ return NOT; }
t(?i:rue)		{ cool_yylval.boolean = 1;
			  return BOOL_CONST; }
f(?i:alse)		{ cool_yylval.boolean = 0;
			  return BOOL_CONST; }

 /*
  *  Identifiers
  */

{LOWER}{IDCHAR}*	{ cool_yylval.symbol = idtable.add_string(yytext);
			  return OBJECTID; }
{UPPER}{IDCHAR}*	{ cool_yylval.symbol = idtable.add_string(yytext);
			  return TYPEID; }

 /*
  * Single character tokens
  */
\.|\@|\~|\*|\/|\+|\-	{ return yytext[0]; }
\<|\=|\(|\)|\{|\}|\;	{ return yytext[0]; }
\:|\,			{ return yytext[0]; }

 /*
  * Multi-character tokens
  */
"<-"			{ return ASSIGN; }
"<="			{ return LE; }
"=>"			{ return DARROW; }


 /*
  *  String constants (C syntax)
  *  Escape sequence \c is accepted for all characters c. Except for 
  *  \n \t \b \f, the result is c.
  *
  */
\"			{ BEGIN(string);
			  mylen = 0;
			  string_buf_ptr = string_buf; }
<string>{LINE}		{ BEGIN(INITIAL);
			  cool_yylval.error_msg = "Unterminated string constant";
			  yyless(0);
			  return ERROR; }
<string><<EOF>>		{ BEGIN(INITIAL);
			  cool_yylval.error_msg = "EOF in string constant";
			  return ERROR; }
<string>\0.*\"		{ BEGIN(INITIAL);
			  cool_yylval.error_msg = "String contains null character";
			  return ERROR; }
<string>\\{LINE}	{ *string_buf_ptr++ = '\n';
			  curr_lineno++; }
<string>\\n		{ *string_buf_ptr++ = '\n'; }
<string>\\t		{ *string_buf_ptr++ = '\t'; }
<string>\\b		{ *string_buf_ptr++ = '\b'; }
<string>\\f		{ *string_buf_ptr++ = '\f'; }
<string>\\[^\0\n]	{ *string_buf_ptr++ = yytext[1]; }
<string>\\		{ *string_buf_ptr++ = '\\'; }
<string>[^\\\n\"\0]+	{ char *cptr = yytext;
			  while(*cptr)
			    *string_buf_ptr++ = *cptr++; }
<string>\"		{ BEGIN(INITIAL);
			  *string_buf_ptr = '\0';
			  cool_yylval.symbol = stringtable.add_string(string_buf);
			  return STR_CONST; }

 /*
  * Comments
  */
"(*"			{ BEGIN(comment);
			  comment_depth++; }
<comment>"(*"		{ comment_depth++; }
<comment>"*)"		{ comment_depth--;
			  if (comment_depth == 0)
			     BEGIN(INITIAL); }
"*)"			{ cool_yylval.error_msg = "Unmatched *)";
			  return ERROR; }
<comment>{LINE}		{ curr_lineno++; }
<comment><<EOF>>	{ BEGIN(INITIAL);
			  cool_yylval.error_msg = "EOF in comment";
			  return ERROR; }
<comment>[^(*\n]*	{ /* do nothing -- mostly everything */ }
<comment>\*[^*)\n]*	{ /* do nothing -- * not followed by )*/ }
<comment>\([^*\n]*      { /* do nothing -- ( not followed by * */ }
"--".*			{ /* do nothing single line comment */ }

 /*
  * Catch single char errors
  */
.			{ cool_yylval.error_msg = yytext;
			  return ERROR; }

 /* 
  * End of file
  */
<<EOF>>			{ yyterminate(); }

