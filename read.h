#ifndef PARSE_H
#define PARSE_H

#include "gew.h"
#include "types.h"

char *show_eval_error(EvalError err);
ASTs read(char text[], EvalError *err);
ASTs read_range(char text[], int start, int end, EvalError *err);
bool find_token(char text[], int *start, int *end, EvalError *err);
void print_asts(ASTs asts, bool top_level);
AST token_from_tokens(ASTs asts);
AST atom_from_text(char *text);

#endif
