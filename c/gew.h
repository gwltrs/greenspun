#ifndef GEW_H
#define GEW_H

#include <stdbool.h>
#include <stddef.h>

// Loop macros

#define for_each(t, e, a) \
for (size_t _i_##e = 0, _j_##e = 1; _i_##e < (a).count; _i_##e++, _j_##e = 1) \
for (t e = (a).array[_i_##e]; _j_##e; _j_##e = 0)

#define for_each_with_i(t, e, a) \
for (size_t i = 0, _j_##e = 1; i < (a).count; i++, _j_##e = 1) \
for (t e = (a).array[i]; _j_##e; _j_##e = 0)

#define for_i(n) for (size_t i = 0; i < (n); i++)
#define for_j(n) for (size_t j = 0; j < (n); j++)
#define for_k(n) for (size_t k = 0; k < (n); k++)

// Dynamic array macros

#define da_new_type(a, e) typedef struct a { e *array; int count; int capacity; } a;
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

#endif