#include <stdio.h>
#include <stdint.h>
#include <malloc.h>

struct node {
    uint32_t value;
    struct node *next;
    struct node *prev;
};

struct node *clockwise(struct node *n, uint32_t amount) {
    for (uint32_t i = 0; i < amount; i++) {
        n = n->next;
    }
    return n;
}

struct node *anticlockwise(struct node *n, uint32_t amount) {
    for (uint32_t i = 0; i < amount; i++) {
        n = n->prev;
    }
    return n;
}

void free_circular(struct node *n) {
    (n->prev)->next = (void *) 0;
    while (n->next) {
        n = n->next;
        free(n->prev);
    }
    free(n);
    return;
}

uint32_t play_game(uint32_t n_players, uint32_t last_marble) {
    uint32_t *scores = calloc(n_players, sizeof(uint32_t)); /* Initially 0 */
    /* place a single marble, 0, in the circle */
    struct node *marbles = calloc(1, sizeof(struct node));
    marbles->value = 0;
    marbles->next = marbles;
    marbles->prev = marbles;
    /* Run the game */
    for (uint32_t marble_val = 1; marble_val <= last_marble; marble_val++) {
        if (marble_val % 23 == 0) {
            /* Something entirely different */
            struct node *position_m6 = anticlockwise(marbles, 6);
            struct node *position_m7 = anticlockwise(marbles, 7);
            struct node *position_m8 = anticlockwise(marbles, 8);
            const uint32_t which_player = (marble_val - 1) % n_players;
            scores[which_player] += marble_val;
            scores[which_player] += (position_m7->value);
            free(position_m7);
            position_m8->next = position_m6;
            position_m6->prev = position_m8;
            marbles = position_m6;
        } else {
            /* Insert new marble between positions 1 and 2 */
            struct node *position_1 = clockwise(marbles, 1);
            struct node *position_2 = clockwise(marbles, 2);
            struct node *new_marble = calloc(1, sizeof(struct node));
            new_marble->value = marble_val;
            new_marble->next = position_2;
            position_2->prev = new_marble;
            new_marble->prev = position_1;
            position_1->next = new_marble;
            marbles = new_marble;
        }
    }
    /* Return the max score */
    uint32_t max_score = 0;
    for (uint32_t i = 0; i < n_players; i++) {
        if (scores[i] > max_score)
            max_score = scores[i];
    }
    free(scores);
    free_circular(marbles);
    return max_score;
}

void get_numbers(FILE *fp, uint32_t *n_players, uint32_t *last_marble) {
    fscanf(fp,
        "%u players; last marble is worth %u points",
        n_players, last_marble
    );
    return;
}

int main(void) {
    FILE *fp = fopen("input09.txt", "r");
    uint32_t n_players, last_marble;
    get_numbers(fp, &n_players, &last_marble);
    fclose(fp);
    /* part 1 */
    uint32_t result = play_game(n_players, last_marble);
    printf("%u\n", result);
    /* part 2 */
    last_marble *= 100;
    result = play_game(n_players, last_marble);
    printf("%u\n", result);
    return 0;
}
