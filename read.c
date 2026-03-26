#include "read.h"

#include "string.h"
#include "stdlib.h"
#include "utils.h"
#include "stdio.h"

ASTs read(char text[], CompileError *err) {
    return read_range(text, 0, strlen(text) - 1, err);
}

ASTs read_range(char text[], int start, int end, CompileError *err) {
    int start_ = start, end_ = end;
    ASTs ts = da_empty(ASTs);
    while (find_token(text, &start_, &end_, err)) {
        if (*err) { da_free(ts); return da_empty(ASTs); }
        if (text[start_] == '(') {
            da_push(ts, list(read_range(text, start_ + 1, end_ - 1, err)));
        } else {
            da_push(ts, atom(substring(text, start_, end_)));
        }
        start_ = end_ + 1;
        end_ = end;
    }
    return ts;
}

bool find_token(char text[], int *start, int *end, CompileError *err) {
    if (*start > *end) return false;
    while (text[*start] == ' ' || text[*start] == '\n') {
        if (*start == *end) return false;
        (*start)++;
    }
    if (text[*start] == ')') { *err = UNBALANCED_PARENS; return false; } 
    else if (text[*start] == '\0') { *err = UNEXPECTED_END_OF_STRING; return false; }
    else if (text[*start] == '(') {
        int depth = 1;
        bool in_quotes = false;
        *end = *start;
        while (depth > 0) {
            (*end)++;
            if (text[*end] == '(' && !in_quotes) depth++;
            if (text[*end] == ')' && !in_quotes) depth--;
            if (text[*end] == '\0') { *err = UNBALANCED_PARENS; return false; }
            if (text[*end] == '"') in_quotes = !in_quotes;
        }
        return true;
    } else if (text[*start] == '"') {
        *end = *start + 1; 
        while (text[*end] != '"') {
            if (text[*end] == '\n' || text[*end] == '\0') { *err = UNBALANCED_QUOTES; return false; }
            (*end)++;
        }
        return true;
    } else {
        *end = *start; 
        while ((33 <= text[*end + 1] && text[*end + 1] <= 126) && text[*end + 1] != '(' && text[*end + 1] != ')') (*end)++;
        return true;
    }
}

void print_asts(ASTs asts, bool top_level) {
    for (int i = 0; i < asts.count; i++) {
        if (i > 0 && !top_level) printf(" ");
        if (asts.array[i].tag == AST_ATOM) {
            printf("%s", asts.array[i].union_.atom);
        } else {
            printf("(");
            print_asts(asts.array[i].union_.list, false);
            if (top_level) { printf(")\n"); } else { printf(")"); };
        }
    }
}

AST list(ASTs asts) { 
    return ((AST){ .tag = AST_LIST, .union_ = { .list = asts }});
}

AST atom(char *text) { 
    return ((AST){ .tag = AST_ATOM, .union_ = { .atom = text }});
}

bool is_visible(char c) { return 33 <= c && c <= 126; }
bool is_upper_alpha(char c) { return 65 <= c && c <= 90; }
bool is_lower_alpha(char c) { return 97 <= c && c <= 122; }
bool is_digit(char c) { return 48 <= c && c <= 57; }