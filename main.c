// if cc *.c -ltcc -o Greenspun; then ./Greenspun; else; fi

#include <stdio.h>
#include <stdlib.h>
#include "libtcc.h"
#include "string.h"

typedef int (*Fun_I32_I32_I32)(int, int); 

struct Chars { char *array; int count; int capacity; };

#define BUFFER_SIZE 4096

TCCState* compile(char *text);

int main(int c, char** a) {

    TCCState *state = compile( 
        "int add(int a, int b) { return a + b; }"
        "int max(int a, int b) { return (a > b) ? a : b; }"
    );

    Fun_I32_I32_I32 add = tcc_get_symbol(state, "add");
    Fun_I32_I32_I32 max = tcc_get_symbol(state, "max");

    printf("%d\n", add(max(-3, 3), 3));

    return 0;
}

TCCState* compile(char *text) {
    TCCState *s = tcc_new();
    tcc_set_output_type(s, TCC_OUTPUT_MEMORY);
    tcc_compile_string(s, text);
    tcc_relocate(s);
    return s;
}