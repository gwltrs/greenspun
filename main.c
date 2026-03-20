// if cc main.c -o Greenspun; then ./Greenspun; else; fi
// #include <stdio.h>
// #include <stdlib.h>
// #include "utils.h"
// #include "parse.h"
#include <stdio.h>
#include "libtcc.h"


int main(int c, char** a) {

    TCCState *s = tcc_new();
    tcc_set_output_type(s, TCC_OUTPUT_MEMORY);

    tcc_compile_string(s, "int add(int a, int b) { return a + b; }");

    tcc_relocate(s/*, TCC_RELOCATE_AUTO*/);

    int (*add)(int, int) = tcc_get_symbol(s, "add");
    printf("%d\n", add(2, 3));

    return 0;
}