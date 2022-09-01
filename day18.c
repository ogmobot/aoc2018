/* File created 2022-08-24 */
#include <malloc.h>
#include <stdio.h>
#include <stdint.h>
#include <unistd.h>

#define ONE_BILLION 1000000000
#define MAX_PERIOD 1024

enum cell {
    S_OPEN = '.',
    S_TREE = '|',
    S_YARD = '#',
    S_NONE = '?',
};

struct universe {
    size_t size;
    size_t cb; /* current buffer -- swaps between 0 and 1 */
    enum cell *cells[2];
};

struct cycle {
    size_t start;  /* time at which cycle begins */
    size_t period; /* max 1024 */
    size_t values[MAX_PERIOD]; /* resource values at each point */
};

struct universe *load_universe(char *filename) {
    /* Assume square universe */
    struct universe *u = calloc(1, sizeof(struct universe));
    u->size = 0;
    u->cb = 0;
    u->cells[0] = calloc(1, sizeof(enum cell) * 1024);
    FILE *fp = fopen(filename, "r");
    size_t index = 0;
    for (int c = fgetc(fp); c != EOF; c = fgetc(fp)) {
        switch (c) {
        case S_OPEN:
        case S_TREE:
        case S_YARD:
            (u->cells[0])[index++] = c;
            break;
        case '\n':
            if (u->size == 0) {
                u->size = index;
                u->cells[0] = reallocarray(
                    u->cells[0],
                    (u->size) * (u->size),
                    sizeof(enum cell));
                u->cells[1] = calloc((u->size) * (u->size), sizeof(enum cell));
            }
            break;
        default:
            printf("Error: unknown character \'%c\'.\n", c);
            break;
        }
    }
    fclose(fp);
    return u;
}

enum cell cell_at(struct universe *u, int32_t row, int32_t col) {
    if (row < 0 || row >= u->size || col < 0 || col >= u->size)
        return S_NONE;
    return *(u->cells[u->cb] + (row * u->size) + col);
}

void update_universe(struct universe *u) {
    enum cell *new_buffer = u->cells[1 - (u->cb)];
    for (int32_t row = 0; row < u->size; row++) {
        for (int32_t col = 0; col < u->size; col++) {
            size_t adj_opentreeyard[3] = {0, 0, 0};
            for (int32_t dr = -1; dr <= 1; dr++) {
                for (int32_t dc = -1; dc <= 1; dc++) {
                    if (dr == 0 && dc == 0)
                        continue;
                    switch (cell_at(u, row + dr, col + dc)) {
                    case S_OPEN:
                        adj_opentreeyard[0]++;
                        break;
                    case S_TREE:
                        adj_opentreeyard[1]++;
                        break;
                    case S_YARD:
                        adj_opentreeyard[2]++;
                        break;
                    }
                }
            }
            enum cell *new_cell = new_buffer + (row * u->size) + col;
            /*
            printf("Cell at (row=%d,col=%d): [%d,%d,%d]\n", row, col,
                adj_opentreeyard[0], adj_opentreeyard[1], adj_opentreeyard[2]);
            */
            switch (cell_at(u, row, col)) {
            case S_OPEN:
                if (adj_opentreeyard[1] >= 3) {
                    *new_cell = S_TREE;
                } else {
                    *new_cell = S_OPEN;
                }
                break;
            case S_TREE:
                if (adj_opentreeyard[2] >= 3) {
                    *new_cell = S_YARD;
                } else {
                    *new_cell = S_TREE;
                }
                break;
            case S_YARD:
                if (adj_opentreeyard[1] >= 1 && adj_opentreeyard[2] >= 1) {
                    *new_cell = S_YARD;
                } else {
                    *new_cell = S_OPEN;
                }
                break;
            case S_NONE:
            default:
                printf("Error! Unrecognised character in grid!\n");
            }
        }
    }
    u->cb = 1 - (u->cb);
    return;
}

size_t resource_value(struct universe *u) {
    size_t n_tree = 0, n_yard = 0;
    for (size_t row = 0; row < u->size; row++) {
        for (size_t col = 0; col < u->size; col++) {
            switch (cell_at(u, row, col)) {
            case S_TREE:
                n_tree++;
                break;
            case S_YARD:
                n_yard++;
                break;
            }
        }
    }
    return n_tree * n_yard;
}

void print_universe(struct universe *u) {
    for (size_t row = 0; row < u->size; row++) {
        for (size_t col = 0; col < u->size; col++) {
            /*printf("%c", *(u->cells[u->cb] + (row * u->size) + col));*/
            printf(" %c", cell_at(u, row, col));
        }
        printf("\n");
    }
    return;
}

struct cycle find_cycle(struct universe *u) {
    /* Determines the period of a cycle in this universe */
    struct cycle result;

    const size_t start = 1000; /* should be large enough for cycle to start */
    const size_t tolerance = 5; /* this many values need to match */

    for (size_t i = 0; i < start; i++)
        update_universe(u);
    for (size_t i = 0; i < MAX_PERIOD; i++) {
        result.values[i] = resource_value(u);
        update_universe(u);
    }
    size_t period = 0;
    uint8_t keep_going = 1;
    while (keep_going) {
        period++;
        keep_going = 0;
        for (size_t i = 0; i < tolerance; i++) {
            if (result.values[i] != result.values[period + i]) {
                keep_going = 1;
                break;
            }
        }
    }
    result.start = start;
    result.period = period;
    return result;
}

size_t get_cycle_value_at(struct cycle cy, size_t t) {
    return cy.values[(t - cy.start) % cy.period];
}

int main(void) {
    /* part 1 */
    struct universe *u = load_universe("input18.txt");
    size_t timer = 0;
    for (size_t i = 0; i < 10; i++) {
        update_universe(u);
        /*
        printf("\33[%luF", u->size);
        print_universe(u);
        usleep(100000);
        timer++;
        */
    }
    printf("%lu\n", resource_value(u));
    /* part 2 */
    struct cycle cy = find_cycle(u);
    printf("%lu\n", get_cycle_value_at(cy, ONE_BILLION - 10));
    free(u->cells[0]);
    free(u->cells[1]);
    free(u);
    return 0;
}
