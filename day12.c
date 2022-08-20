#include <stdio.h>
#include <stdint.h>
#include <malloc.h>

#define num_t int32_t
#define STARTING_CAPACITY 256
#define NUM_RULES 32

struct buffer {
    size_t capacity;
    size_t length;
    num_t *array;
    num_t *active; /* used to search */
};

struct buffer *new_buffer(void) {
    struct buffer *b = calloc(1, sizeof(struct buffer));
    b->capacity = STARTING_CAPACITY;
    b->length = 0;
    b->array = calloc(b->capacity, sizeof(num_t));
    b->active = b->array;
    return b;
}

void clear(struct buffer *b) {
    b->length = 0;
    b->active = b->array;
    return;
}

void append(struct buffer *b, num_t x) {
    b->array[(b->length)++] = x;
    if (b->length >= b->capacity) {
        num_t *tmp = realloc(b->array, (b->capacity * 2) * sizeof(num_t));
        if (tmp) {
            b->array = tmp;
            b->capacity *= 2;
        } else {
            printf("WARNING: Failed to realloc array memory!\n");
            printf("The next write to buffer at %p may fail!\n", b);
        }
    }
    return;
}

uint8_t contains(struct buffer *b, num_t x) {
    /* Makes big assumptions about the buffer:
     * - Elements are always ascending
     * - After the existence of an element is checked, no element lower
     *   than that will ever be checked again
     */
    if (b->active - b->array >= b->length)
        return 0;
    if (*(b->active) < x)
        (b->active)++;
    return (b->active - b->array < b->length) && *(b->active) == x;
}

void populate(struct buffer *old, struct buffer *new, uint8_t rules[NUM_RULES]) {
    /* Reads from old buffer and writes to new buffer */
    clear(new);
    num_t minimum = (old->array)[0];
    num_t maximum = (old->array)[(old->length) - 1];
    /* Set up sliding window as 5-bit integer */
    uint8_t window = 0;
    for (num_t i = minimum - 2; i < maximum + 2; i++) {
        /* Shift window by one unit. `i` is index in the new buffer,
         * so the front of the window is two units ahead. */
        window = ((0xf & window) << 1) + contains(old, i + 2);
        if (rules[window])
            append(new, i);
    }
    return;
}

void load_file(char *filename, struct buffer *b, uint8_t *rules) {
    char line[256] = {};
    char pattern[6] = {};
    char outcome;
    FILE *fp = fopen(filename, "r");
    /* Populate state */
    fgets(line, 256, fp);
    /* "initial state: "
        0123456789ABCDE == 15 letters long */
    size_t index = 0;
    for (char *c = &(line[15]); *c != '\0'; c++) {
        if (*c == '#')
            append(b, index);
        index++;
    }
    fgets(line, 256, fp); /* Blank line */
    /* Populate rules */
    for (int i = 0; i < NUM_RULES; i++) {
        fgets(line, 256, fp);
        sscanf(line, "%s => %c", pattern, &outcome);
        uint8_t index = 0;
        for (int j = 0; j < 5; j++) {
            index |= (pattern[4 - j] == '#') << j;
        }
        /*printf("%s == %hhd\n", pattern, index);*/
        rules[index] = (outcome == '#');
    }
    return;
}

void display_buffer(struct buffer *b) {
    for (int i = (b->array)[0]; i < (b->array)[b->length - 1]; i++) {
        if (contains(b, i)) {
            printf("#");
        } else {
            printf(".");
        }
    }
    printf("\n");
    b->active = b->array;
    return;
}

num_t buffer_score(struct buffer *b) {
    num_t total = 0;
    for (size_t i = 0; i < b->length; i++) {
        total += (b->array)[i];
    }
    return total;
}

int main(void) {
    static struct buffer *bs[2] = {};
    bs[0] = new_buffer();
    bs[1] = new_buffer();
    static uint8_t rules[NUM_RULES] = {};

    load_file("input12.txt", bs[0], rules);

    /* Part 1 */
    size_t gen_count;
    for (gen_count = 0; gen_count < 20; gen_count++) {
        populate(bs[gen_count % 2], bs[(gen_count + 1) % 2], rules);
        /*printf("Generation %2lu: %6lu\n", gen_count + 1, bs[(gen_count + 1) % 2]->length);*/
        /*printf("%3lu ", gen_count);*/
        /*display_buffer(bs[(gen_count + 1) % 2]);*/
    }
    /* Result should be in bs[0] after an even number of rounds */
    /*printf("Living cells: %lu\n", (bs[0])->length);*/
    printf("%d\n", buffer_score(bs[0]));

    /* Part 2 */
    /* The pattern becomes predictable at generation 127.
       Run up until past that point (198 generations total) */
    for (; gen_count < 198; gen_count++)
        populate(bs[gen_count % 2], bs[(gen_count + 1) % 2], rules);

    /* The total value of the array only changes linearly from now on. */
    num_t score_198 = buffer_score(bs[0]);
    for (; gen_count < 200; gen_count++)
        populate(bs[gen_count % 2], bs[(gen_count + 1) % 2], rules);
    num_t score_200 = buffer_score(bs[0]);

    int64_t delta_score = (int64_t) (score_200 - score_198) / 2;

    /* After 50 billion generations... */
    printf("%ld\n", score_200 + (delta_score * (50000000000 - 200)));

    free((bs[0])->array);
    free(bs[0]);
    free((bs[1])->array);
    free(bs[1]);
    return 0;
}
