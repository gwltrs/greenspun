#ifndef PARSE_H
#define PARSE_H

#include "gew.h"
#include "types.h"

ASTs read(char text[], CompileError *err);
ASTs read_range(char text[], int start, int end, CompileError *err);
bool find_token(char text[], int *start, int *end, CompileError *err);
void print_asts(ASTs asts, bool top_level);
AST list(ASTs asts);
AST atom(char *text);
bool is_visible(char c);
bool is_upper_alpha(char c);
bool is_lower_alpha(char c);
bool is_digit(char c);

#endif
