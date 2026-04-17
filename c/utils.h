#ifndef UTILS_H
#define UTILS_H

#include "stdbool.h"
#include "math.h"

#define da_empty(t) ((t){ .array = NULL, .count = 0, .capacity = 0 })
#define da_push(a, e) \
    do { \
        if ((a).count == (a).capacity) { \
            if ((a).capacity == 0) (a).capacity = 4; \
            else (a).capacity *= 2; \
            (a).array = realloc((a).array, (a).capacity * sizeof(*(a).array)); \
        } \
        (a).array[(a).count] = (e); \
        (a).count++; \
    } while (0);
#define da_free(a) if ((a).array) { free((a).array); (a).array = NULL; }
#define da_delete_fast(a, i) \
    do { \
        if ((a).count == 0 || (i) < 0) return; \
        if ((i) < (a).count - 1) (a).array[(i)] = (a).array[(a).count - 1]; \
        ((a).count)--; \
    } while (0);

#define min(a, b) ((a) < (b) ? (a) : (b))
#define max(a, b) ((a) > (b) ? (a) : (b))

char *substring(char *source, int start, int end);
bool char_is_num(char c); 
bool ends_with(char *str, char *suffix);
char *drop(char *str, int n);

typedef struct Chars { char *array; int count; int capacity; } Chars;
Chars chars_empty();
void chars_push(Chars *chars, char *str);
char chars_pop(Chars *chars);

char* file_read_all(char *file_name);
bool file_write_all(char *file_name, char *text);

#endif