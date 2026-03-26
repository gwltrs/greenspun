#ifndef TRANSPILE_H
#define TRANSPILE_H

#include "gew.h"
#include "types.h"

// Type type_from_ast(AST ast, CompileError *err);
bool is_valid_type(char *text);
bool is_valid_identifier(char *text);
bool is_bool_literal(AST ast, TypedAST *typed_ast, CompileError *err);
bool is_int64_literal(AST ast, TypedAST *typed_ast, CompileError *err);
TypedAST type_check(AST ast, CompileError *err);
char *transpile(AST ast, int depth);

#endif