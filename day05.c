#include <stdio.h>
#include <string.h>

#define INPUT_SIZE 50000
#define BUFLEN     65536 /* Must be at least INPUT_SIZE + 1 */

char toggle(char c) {
    if (c > 'Z') {
        /* Lowercase (0x41..) */
        return c - 0x20;
    } else {
        /* Uppercase (0x61..) */
        return c + 0x20;
    }
}

int next_nonzero (char buffer[BUFLEN], int index) {
    while ((buffer[index] == '\0') && (index < BUFLEN)) {
        index += 1;
    }
    return index;
}

int remove_pairs(char buffer[BUFLEN]) {
    /* Returns 1 if any pairs were removed, 0 otherwise */
    int ret = 0, last = 0, current = 0;
    last = next_nonzero(buffer, 0);
    current = next_nonzero(buffer, last + 1);
    while (last < BUFLEN) {
        if (toggle(buffer[last]) == buffer[current]) {
            ret = 1;
            buffer[last] = '\0';
            buffer[current] = '\0';
        }
        current = next_nonzero(buffer, last);
        last = next_nonzero(buffer, current + 1);
    }
    return ret;
}

int reduced_length(char buffer[BUFLEN]) {
    /* This MANGLES the buffer -- make a copy! */
    /* Mutate buffer until it stops changing */
    while (remove_pairs(buffer)) ;
    /* Output number of non-zero chars */
    int total = 0;
    for (int i = 0; i < INPUT_SIZE; i++) {
        total += (buffer[i] != '\0');
    }
    return total;
}

int main(void) {
    /* Populate input buffer */
    char buffer[65536] = {0};
    FILE *input = fopen("input05.txt", "r");
    if (!input) { printf("File not found!\n"); return 1; }
    fread(buffer, sizeof(char), INPUT_SIZE, input);
    fclose(input);

    char temp[BUFLEN];

    /* Part 1 */
    memcpy((char *) temp, (const char *) buffer, BUFLEN * sizeof(char));
    printf("%d\n", reduced_length(temp));

    /* Part 2 */
    int best = INPUT_SIZE;
    for (char target = 'A'; target <= 'Z'; target++) {
        /* Set up temp buffer */
        memcpy((char *) temp, (const char *) buffer, BUFLEN * sizeof(char));
        /* Wipe the target character */
        for (int i = 0; i < INPUT_SIZE; i++) {
            if ((temp[i] == target) || (temp[i] == target + 0x20)) {
                temp[i] = '\0';
            }
        }
        /* Test this configuration */
        int test = reduced_length(temp);
        if (test < best) {
            best = test;
        }
    }
    printf("%d\n", best);
    return 0;
}
