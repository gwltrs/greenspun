#include "transpile.h"

#include "read.h"
#include "string.h"

// Type type_from_ast(AST ast, CompileError *err) { if (ast.tag == AST_ATOM) }

bool is_valid_type_name(char *text) {

}

bool is_valid_identifier(char *text) {

}

bool is_bool_literal(AST ast, TypedAST *typed_ast, CompileError *err) {
    if (ast.tag == AST_LIST) return false;
    if (ast.union_.atom != "true" && ast.union_.atom != "false") return false;
    *typed_ast = (TypedAST){ .type = atom("Bool"), .ast = ast };
    return true;
}

bool is_int64_literal(AST ast, TypedAST *typed_ast, CompileError *err) {
    if (ast.tag == AST_LIST) return false;
    char fst_c = ast.union_.atom[0], snd_c = ast.union_.atom[1];
    if (!is_digit(fst_c) && fst_c != '-') return false;
    if (fst_c == '-' && !is_digit(snd_c)) return false;
    for (int i = fst_c == '-' ? 2 : 1; strlen(ast.union_.atom); i++) 
        if (!is_digit(ast.union_.atom[i])) {
            *err = INVALID_INT_LITERAL; 
            return false;
        }
    *typed_ast = (TypedAST){ .type = atom("I64"), .ast = ast };
    return true;
}

TypedAST type_check(AST ast, CompileError *err) {
    if (ast.tag == AST_ATOM) {

    } else {

    }
}


char *transpile(AST ast, int depth) {

    return "int x = 0;";
}