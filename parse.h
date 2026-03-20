#ifndef PARSE_H
#define PARSE_H

#include "stdbool.h"

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

typedef enum { TT_TEXT, TT_ARRAY } TokenType;
struct Tokens;
typedef union { char *text; struct Tokens *array; } TokenUnion;
typedef struct { TokenType type; TokenUnion union_; } Token;
typedef struct Tokens { Token *array; int count; int capacity; } Tokens;

Tokens parse(char text[], EvalError *err);
Tokens parse_range(char text[], int start, int end, EvalError *err);
bool find_token(char text[], int *start, int *end, EvalError *err);
void print_tokens(Tokens tokens);
Token token_from_tokens(Tokens tokens);
Token token_from_text(char *text);

#endif
