#ifndef TYPES_H
#define TYPES_H

#include "gew.h"

typedef enum {
    MISMATCHED_PARENS = 1,
    MISMATCHED_QUOTES,
    UNEXPECTED_END_OF_STRING,
    /*
    EVAL_SYMBOL_UNKNOWN,
    EVAL_ARGS_TOO_FEW,
    EVAL_ARG_WRONG_TYPE,
    EVAL_ARG_BAD_VALUE,
    EVAL_NUM_BAD_FORMAT,
    EVAL_NUM_BAD_VALUE,
    EVAL_APPLY_ON_NON_FUNC,
    */
} EvalError;


typedef enum ASTTag { AST_ATOM, AST_LIST } ASTTag;
typedef union ASTUnion { char *atom; struct ASTs *list; } ASTUnion;
typedef struct AST { ASTTag type; ASTUnion union_; } AST;
typedef struct ASTs { AST *array; int count; int capacity; } ASTs;

typedef enum ScalarType { SCALAR_TYPE_BOOL = 1, SCALAR_TYPE_I32, SCALAR_TYPE_F32 } ScalarType;

typedef enum TypeTag { TYPE_SCALAR = 1, TYPE_COMPOSITIE } TypeTag;
typedef union TypeUnion { ScalarType scalar; struct CompositeType *composite; } TypeUnion;
typedef struct Type { TypeTag tag; TypeUnion union_; } Type;
da_new_type(Types, Type);

typedef struct CompositeType { char *name; Types inner_types; } CompositeType;

// typedef struct Prop { Type type; char *name; } Prop;
// da_new_type(Props, Prop);
// typedef struct Struct { Props props; } Struct;
// typedef struct Union { Props props; } Union;
// typedef struct Ptr { Type inner; } Ptr;

char *show_eval_error(EvalError err);
int compare_types(Type a, Type b);

#endif