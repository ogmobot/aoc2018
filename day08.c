#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <malloc.h>

struct node {
    uint32_t n_children;
    uint32_t n_metadata;
    struct node *children;
    uint32_t *metadata;
};

void populate_next_node(FILE *fp, struct node *dest) {
    /* read header */
    fscanf(fp, "%u", &(dest->n_children));
    fscanf(fp, "%u", &(dest->n_metadata));
    /* read children */
    if (dest->n_children > 0) {
        dest->children = calloc(dest->n_children, sizeof(struct node));
        for (uint32_t i = 0; i < dest->n_children; i++) {
            populate_next_node(fp, (dest->children) + i);
        }
    } else { dest->children = (void *) 0; }
    /* read metadata */
    if (dest->n_metadata > 0) {
        dest->metadata = calloc(dest->n_metadata, sizeof(uint32_t));
        for (uint32_t i = 0; i < dest->n_metadata; i++) {
            fscanf(fp, "%u", (dest->metadata) + i);
        }
    } else { dest->metadata = (void *) 0; }
    return;
}

void free_children_and_metadata(struct node *n) {
    if (n->n_children > 0) {
        for (uint32_t i = 0; i < n->n_children; i++) {
            free_children_and_metadata((n->children) + i);
        }
        free(n->children);
    }
    if (n->n_metadata > 0) {
        free(n->metadata);
    }
}

/* Part 1 */
uint32_t total_metadata(struct node *n) {
    uint32_t total = 0;
    for (uint32_t i = 0; i < n->n_metadata; i++) {
        total += (n->metadata)[i];
    }
    for (uint32_t i = 0; i < n->n_children; i++) {
        total += total_metadata((n->children) + i);
    }
    return total;
}

/* Part 2 */
uint32_t node_value(struct node *n) {
    const bool has_children = (n->n_children > 0);
    uint32_t total = 0;
    for (uint32_t i = 0; i < n->n_metadata; i++) {
        if (has_children) {
            const uint32_t index = (n->metadata)[i] - 1;
            if (index < n->n_children) {
                total += node_value((n->children) + index);
            }
        } else {
            total += (n->metadata)[i];
        }
    }
    return total;
}

int main(void) {
    FILE *fp = fopen("input08.txt", "r");
    struct node *n = calloc(1, sizeof(struct node));
    populate_next_node(fp, n);
    fclose(fp);
    printf("%u\n", total_metadata(n));
    printf("%u\n", node_value(n));
    free_children_and_metadata(n);
    free(n);
    return 0;
}
