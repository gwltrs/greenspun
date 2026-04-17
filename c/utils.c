#include "utils.h"
#include "stdlib.h"
#include "string.h"
#include "stdio.h"

char *substring(char *source, int start, int end) {
    char *s = malloc((end - start + 2) * sizeof(char));
    s[end - start + 1] = '\0';
    strncpy(s, source + start, end - start + 1);
    return s;
}

bool char_is_num(char c) {
    return 48 <= c && c <= 57;
}

bool ends_with(char *str, char *suffix) {
    int str_len = strlen(str), suffix_len = strlen(suffix); 
    if (str_len < suffix_len) return false;
    int i = suffix_len - 1;
    for (int i = 0; suffix_len - 1 - i > 0; i++)
        if (str[str_len - 1 - i] != suffix[suffix_len - 1 - i])
            return false;
    return true;
}

char *drop(char *str, int n) {
    int len = strlen(str);
    if (len <= n) return "";
    return substring(str, 0, len - n - 1);
}

Chars chars_empty() {
    char *str = malloc(1 * sizeof(char));
    str[0] = '\0';
    return (Chars){ .array = str, .count = 0, .capacity = 0 }; 
}

void chars_push(Chars *chars, char *str) {
    int len = strlen(str);
    if (chars->count + len + 1 > chars->capacity) {
        chars->capacity = 2 * (chars->count + len + 1);
        chars->array = realloc(chars->array, chars->capacity * sizeof(char));
    }
    memcpy(chars->array + chars->count, str, len * sizeof(char));
    chars->count += len;
    chars->array[chars->count] = '\0';
}

char chars_pop(Chars *chars) {
    if (chars->count == 0) return '\0';
    char c = chars->array[chars->count - 1];
    chars->array[chars->count - 1] = '\0';
    chars->count--;
    return c;
}

char* file_read_all(char *file_name) {
    FILE* fptr = fopen(file_name, "r");
    if (fptr == NULL) return NULL;
    Chars chars = chars_empty();
    char c;
    while ((c = fgetc(fptr)) != EOF) {
        char str[2] = { '\0', '\0' };
        str[0] = c;
        chars_push(&chars, (char*)&str);
    }
    fclose(fptr);
    return chars.array;
}

bool file_write_all(char *file_name, char *text) {
    FILE* fptr = fopen(file_name, "w");
    if (fptr == NULL) return false;
    fprintf(fptr, "%s", text);
    fclose(fptr);
    return true;
}