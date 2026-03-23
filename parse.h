#ifndef PARSE_H
#define PARSE_H

#include "gew.h"

typedef enum {
    PARSE_MISMATCHED_PARENS = 1,
    PARSE_MISMATCHED_QUOTES,
    PARSE_END_OF_STRING,
    EVAL_SYMBOL_UNKNOWN,
    EVAL_ARGS_TOO_FEW,
    EVAL_ARG_WRONG_TYPE,
    EVAL_ARG_BAD_VALUE,
    EVAL_NUM_BAD_FORMAT,
    EVAL_NUM_BAD_VALUE,
    EVAL_APPLY_ON_NON_FUNC,
} EvalError;

char *show_eval_error(EvalError err);

typedef enum { AST_ATOM, AST_LIST } ASTType;
struct ASTs;
typedef union { char *atom; struct ASTs *list; } ASTUnion;
typedef struct { ASTType type; ASTUnion union_; } AST;
typedef struct ASTs { AST *array; int count; int capacity; } ASTs;

ASTs parse(char text[], EvalError *err);
ASTs parse_range(char text[], int start, int end, EvalError *err);
bool find_token(char text[], int *start, int *end, EvalError *err);
void print_tokens(ASTs asts);
AST token_from_tokens(ASTs asts);
AST atom_from_text(char *text);

#endif
