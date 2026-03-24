// if cc *.c -ltcc -o Greenspun; then ./Greenspun; else; fi

#include <stdio.h>
#include <stdlib.h>
#include "libtcc.h"
#include "string.h"
#include "utils.h"
#include "read.h"

#define BUFFER_SIZE 4096

int main(int c, char** a) {

    char *file_text = file_read_all("example.green");
    if (!file_text) { printf("Failed to read file.\n"); return -1; };
    EvalError err = 0;
    ASTs asts = read(file_text, &err);
    if (err) { printf("%s\n", show_eval_error(err)); return -1; }
    print_asts(asts, true); printf("\n");

    return 0;
}

// typedef int (*Fun_I32_I32_I32)(int, int); 

TCCState* compile(char *text) {

    /*
    TCCState *state = compile( 
        "int add(int a, int b) { return a + b; }"
        "int max(int a, int b) { return (a > b) ? a : b; }"
    );
    Fun_I32_I32_I32 add = tcc_get_symbol(state, "add");
    Fun_I32_I32_I32 max = tcc_get_symbol(state, "max");
    printf("%d\n", add(max(-3, 3), 3));
    */

    TCCState *s = tcc_new();
    tcc_set_output_type(s, TCC_OUTPUT_MEMORY);
    tcc_compile_string(s, text);
    tcc_relocate(s);
    return s;
}