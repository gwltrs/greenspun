#include "types.h"

char *show_compile_error(CompileError err) {
    if (err == UNBALANCED_PARENS) return "UNBALANCED_PARENS"; 
    if (err == UNBALANCED_QUOTES) return "UNBALANCED_QUOTES"; 
    if (err == UNEXPECTED_END_OF_STRING) return "UNEXPECTED_END_OF_STRING"; 
    if (err == INVALID_NAMING) return "INVALID_NAMING";
    if (err == INVALID_INT_LITERAL) return "INVALID_INT_LITERAL";
    /*
    if (err == EVAL_SYMBOL_UNKNOWN) return "EVAL_SYMBOL_UNKNOWN"; 
    if (err == EVAL_ARGS_TOO_FEW) return "EVAL_ARGS_TOO_FEW"; 
    if (err == EVAL_ARG_WRONG_TYPE) return "EVAL_ARG_WRONG_TYPE"; 
    if (err == EVAL_ARG_BAD_VALUE) return "EVAL_ARG_BAD_VALUE"; 
    if (err == EVAL_NUM_BAD_FORMAT) return "EVAL_NUM_BAD_FORMAT"; 
    if (err == EVAL_NUM_BAD_VALUE) return "EVAL_NUM_BAD_VALUE"; 
    if (err == EVAL_APPLY_ON_NON_FUNC) return "EVAL_APPLY_ON_NON_FUNC"; 
    */
    return "";
}

/*
int compare_types(Type a, Type b) {
    if (a.tag != b.tag) return a.tag - b.tag;
    if (a.tag == TYPE_SCALAR) return a.union_.scalar - b.union_.scalar;
    if (a.union_.composite.inner_types.count != b.union_.composite.inner_types.count)
        return a.union_.composite.inner_types.count - b.union_.composite.inner_types.count;
    for_i (a.union_.composite.inner_types.count) {
        int inner = compare_types(a.union_.composite.inner_types.array[i], b.union_.composite.inner_types.array[i]);
        if (inner) return inner;
    }
    return 0;
}
*/

Class class(AST ast, CompileError *err) {
    if (ast.tag == AST_ATOM) {

    } else {

    }
}