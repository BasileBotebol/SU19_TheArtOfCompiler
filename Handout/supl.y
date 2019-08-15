/*------------------------------------------------------------------------------

  HEIG-Vd - CoE@SNU              Summer University             August 5-16, 2019

  The Art of Compiler Construction


  suPL parser skeleton


  @brief snPL parser
  @author Bernhard Egger <bernhard@csap.snu.ac.kr>
  @section changelog Change Log
  2016/07/10 bernhard created
  2019/08/01 bernhard adapted to long ints

  @section license_section License
  Copyright (c) 2016-2019, Computer Systems and Platforms Laboratory, SNU
  All rights reserved.

  Redistribution and use in source and binary forms,  with or without modifi-
  cation, are permitted provided that the following conditions are met:

  - Redistributions of source code must retain the above copyright notice,
    this list of conditions and the following disclaimer.
  - Redistributions in binary form must reproduce the above copyright notice,
    this list of conditions and the following disclaimer in the documentation
    and/or other materials provided with the distribution.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING,  BUT NOT LIMITED TO,  THE
  IMPLIED WARRANTIES OF MERCHANTABILITY  AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDER  OR CONTRIBUTORS BE
  LIABLE FOR ANY DIRECT,  INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSE-
  QUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF  SUBSTITUTE
  GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN  CONTRACT, STRICT
  LIABILITY, OR TORT  (INCLUDING NEGLIGENCE OR OTHERWISE)  ARISING IN ANY WAY
  OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
  DAMAGE.

------------------------------------------------------------------------------*/


%locations

%code top{
#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <assert.h>

#define YYDEBUG 1

extern char *yytext;
}

%code requires {
#include "supllib.h"
}

%union {
  long int lval;
  char     *str;
  IDlist   *idl;
  EType    t;
  EOpcode opcode;
  BPrecord *bpr;
}

%code {
  Stack   *stack = NULL;
  Symtab *symtab = NULL;
  Funclist  *fnl = NULL;
  CodeBlock  *cb = NULL;

  char *fn_pfx   = NULL;
  EType rettype  = tVoid;
}

%start program

%token LONG VOID
%token INTVAL
%token IDENT
%token IF
%token ELSE
%token WHILE
%token RETURN

%token READ
%token WRITE
%token PRINT

%token EQ LE LT
%token STRING


%type<lval> INTVAL
%type<str>  ident IDENT STRING
%type<t>    type
%type<idl>  identl vardecl
%type<lval> argl
%type<opcode> condition
%type<bpr> IF WHILE

%%

program     :                                 { stack = init_stack(NULL); symtab = init_symtab(stack, NULL);
                                                fnl = NULL;
                                              }
              decll                           { cb = init_codeblock("");
                                                stack = init_stack(stack); symtab = init_symtab(stack, symtab);
                                                rettype = tVoid;
                                              }
              stmtblock                       { add_op(cb, opHalt, NULL);
                                                dump_codeblock(cb); save_codeblock(cb, fn_pfx);
                                                Stack *pstck = stack; stack = stack->uplink; delete_stack(pstck);
                                                Symtab *pst = symtab; symtab = symtab->parent; delete_symtab(pst);

                                                delete_stack(stack); delete_symtab(symtab);
                                                delete_funclist(fnl); delete_codeblock(cb);
                                              }
            ;

decll       : %empty
            | decll vardecl ';'               { delete_idlist($vardecl); }
            | decll fundecl
            ;

vardecl     : %empty                          { $$ = NULL; }
            | type identl                     { if($type == tVoid){
                                                  char *error = NULL;
                                                  asprintf(&error, "Void type variable");
                                                  yyerror(error);
                                                  free(error);
                                                  YYABORT;
                                                }

                                                IDlist *l = $identl;
                                                while (l) {
                                                  if (insert_symbol(symtab, l->id, $type) == NULL) {
                                                    char *error = NULL;
                                                    asprintf(&error, "Duplicated identifier '%s'.", l->id);
                                                    yyerror(error);
                                                    free(error);
                                                    YYABORT;
                                                  }
                                                  l = l->next;
                                                }
                                                $$ = $identl;
                                              }
            ;

type        : LONG                            { $$ = tLong; }
            | VOID                            { $$ = tVoid; }
            ;

identl      : ident                           { $$ = (IDlist*)calloc(1, sizeof(IDlist)); $$->id = $ident; }
            | identl ',' ident                { $$ = (IDlist*)calloc(1, sizeof(IDlist)); $$->id = $ident; $$->next = $1; }
            ;

ident       : IDENT
            ;

fundecl     : type ident '('                  { 
                                                cb = init_codeblock($ident);
                                                rettype = $type;
                                                stack = init_stack(stack);
                                                symtab = init_symtab(stack, symtab);
                                              }
              vardecl ')'                     { 
                                                int argc = 0;
                                                while($vardecl){
                                                  argc ++;
                                                  $vardecl = $vardecl->next;
                                                }

                                                if(find_func(fnl, $ident) == NULL){
                                                  Funclist* FLelem = (Funclist*)calloc(1, sizeof(Funclist));
                                                  FLelem->id = $ident;
                                                  FLelem->rettype = $type;
                                                  FLelem->narg = argc;
                                                  if(fnl == NULL){
                                                    fnl = FLelem;
                                                  }else{
                                                    FLelem->next = fnl;
                                                    fnl = FLelem;
                                                  }

                                                }else{
                                                  char *error = NULL;
                                                  asprintf(&error, "Duplicated function identifier '%s'.", $ident);
                                                  yyerror(error);
                                                  free(error);
                                                  YYABORT;
                                                }
                                              }
              stmtblock                       {
                                                add_op(cb, opReturn, NULL);
                                                dump_codeblock(cb); save_codeblock(cb, fn_pfx);
                                                Stack *pstck = stack; stack = stack->uplink; delete_stack(pstck);
                                                Symtab *pst = symtab; symtab = symtab->parent; delete_symtab(pst);
                                              }
            ;

stmtblock   : '{' stmts '}'
            ;

stmts       : stmt
            | stmt stmts
            ;

stmt        : %empty
            | vardecl ';'
            | assign
            | if
            | while
            | call ';'
            | return
            | read
            | write
            | print
            ;

assign      : ident '=' expression ';'        {
                                                Symbol *s = find_symbol(symtab, $ident, sLocal);
                                                if (s == NULL) {
                                                  char *error = NULL;
                                                  asprintf(&error, "Unknown identifier '%s'.", $ident);
                                                  yyerror(error);
                                                  free(error);
                                                  YYABORT;
                                                }
                                                add_op(cb, opStore, s);
                                              }
            ;

if          : IF '(' condition ')'            {
                                                $IF = (BPrecord*)calloc(1, sizeof(BPrecord));
                                                Operation *tp = add_op(cb, $condition, (void*)OPID_INVALID);
                                                Operation *fb = add_op(cb, opJump, (void*)OPID_INVALID);
                                                $IF->ttrue = add_backpatch($IF->ttrue, tp);
                                                $IF->tfalse = add_backpatch($IF->tfalse, fb);
                                                pending_backpatch(cb, $IF->ttrue);
                                              }
              stmtblock                       {
                                                Operation *next = add_op(cb, opJump, (void*)OPID_INVALID);
                                                $IF->end = add_backpatch($IF->end, next);
                                                pending_backpatch(cb, $IF->tfalse);
                                              }
              else                            {
                                                pending_backpatch(cb, $IF->end);
                                              }
            ;

else        : %empty
            | ELSE stmtblock
            ;

while       : WHILE                           {
                                                $WHILE = (BPrecord*)calloc(1,sizeof(BPrecord));
                                                $WHILE->pos = cb->nops;
                                              }
            '(' condition ')'                 {
                                                Operation *tb = add_op(cb,$condition,(void*)OPID_INVALID);
                                                Operation *next = add_op(cb,opJump,(void*)OPID_INVALID);
                                                $WHILE->ttrue = add_backpatch($WHILE->ttrue,tb);
                                                $WHILE->end = add_backpatch($WHILE->end,next);
                                                pending_backpatch(cb,$WHILE->ttrue);
                                              }
            stmtblock                         {
                                                add_op(cb,opJump,(void*)(long int)$WHILE->pos);
                                                pending_backpatch(cb,$WHILE->end);
                                              }
            ;

call        : ident '(' argl ')'              { 
                                                Funclist* functioncalled = find_func(fnl, $ident);
                                                if(functioncalled == NULL){
                                                  char *error = NULL;
                                                  asprintf(&error, "Unknown function identifier '%s'.", $ident);
                                                  yyerror(error);
                                                  free(error);
                                                  YYABORT;
                                                }else if(functioncalled->narg != $argl){
                                                  char *error = NULL;
                                                  asprintf(&error, "Invalid number of argument for call to %s. Expected %d, received %d", $ident, functioncalled->narg, $argl);
                                                  yyerror(error);
                                                  free(error);
                                                  YYABORT;
                                                }
                                                add_op(cb, opCall, functioncalled->id);
                                              }
            ;

return      : RETURN ';'                      {
                                                if (rettype!= tVoid) {
                                                  yyerror("No return in non-void function.");
                                                  YYABORT;
                                                }
                                              }
            | RETURN expression ';'           {
                                                if (rettype == tVoid) {
                                                yyerror("Void function returning non-void expression.");
                                                YYABORT;
                                                }
                                              }
            ;


read        : READ ident ';'                  { 
                                                Symbol *s = find_symbol(symtab, $ident, sLocal);
                                                if (s == NULL) {
                                                  char *error = NULL;
                                                  asprintf(&error, "Unknown identifier '%s'.", $ident);
                                                  yyerror(error);
                                                  free(error);
                                                  YYABORT;
                                                }
                                                add_op(cb, opRead, s);
                                              }
            ;

write       : WRITE expression ';'            { add_op(cb, opWrite, NULL); }
            ;

print       : PRINT STRING ';'                { add_op(cb, opPrint, $STRING); }
            ;

expression  : number
            | ident                           { 
                                                Symbol *s = find_symbol(symtab, $ident, sLocal);
                                                if (s == NULL) {
                                                  char *error = NULL;
                                                  asprintf(&error, "Unknown identifier '%s'.", $ident);
                                                  yyerror(error);
                                                  free(error);
                                                  YYABORT;
                                                }
                                                add_op(cb, opLoad, (void*) s);
                                              }
            | expression '+' expression       { add_op(cb, opAdd, NULL); }
            | expression '-' expression       { add_op(cb, opSub, NULL); }
            | expression '*' expression       { add_op(cb, opMul, NULL); }
            | expression '/' expression       { add_op(cb, opDiv, NULL); }
            | expression '%' expression       { add_op(cb, opMod, NULL); }
            | expression '^' expression       { add_op(cb, opPow, NULL); }
            | '(' expression ')'
            | call
            ;

argl        : %empty                          { $$ = 0; }
            | expression                      { $$ = 1; }
            | argl ',' expression             { $$++; }
            ;


condition   : expression EQ expression        { $$ = opJeq; }
            | expression LE expression        { $$ = opJle; }
            | expression LT expression        { $$ = opJlt; }
            ;

number      : INTVAL                          { add_op(cb, opPush, (void*) $INTVAL); }
            ;

%%

int main(int argc, char *argv[])
{
  extern FILE *yyin;
  argv++; argc--;

  while (argc > 0) {
    // prepare filename prefix (cut off extension)
    fn_pfx = strdup(argv[0]);
    char *dot = strrchr(fn_pfx, '.');
    if (dot != NULL) *dot = '\0';

    // open source file
    yyin = fopen(argv[0], "r");
    yydebug = 0;

    // parse
    yyparse();

    // next input
    free(fn_pfx);
    argv++; argc--;
  }

  return EXIT_SUCCESS;
}

int yyerror(const char *msg)
{
  printf("Parse error at %d:%d: %s\n", yylloc.first_line, yylloc.first_column, msg);
  return EXIT_FAILURE;
}
