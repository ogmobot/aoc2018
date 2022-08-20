#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <malloc.h>

/* By observation: */
#define MAP_SIZE 150

#define S_EMPTY ' '
#define S_H_STRAIGHT  '-'
#define S_V_STRAIGHT  '|'
#define S_NE_SW_CURVE '/'
#define S_NW_SE_CURVE '\\'
#define S_INTERSECTION '+'
#define S_TRAIN_E '>'
#define S_TRAIN_N '^'
#define S_TRAIN_W '<'
#define S_TRAIN_S 'v'

struct node {
    char symbol;
    struct minecart *cart;
};

struct minecart {
    bool alive;
    enum {
        D_EAST  = 0,
        D_NORTH = 1,
        D_WEST  = 2,
        D_SOUTH = 3
    } dir;
    enum {
        T_LEFT  = 0,
        T_AHEAD = 1,
        T_RIGHT = 2
    } next_turn;
    uint64_t age;
};

void turn_left(struct minecart *cart) {
    cart->dir = (cart->dir + 1) % 4;
    return;
}
void turn_right(struct minecart *cart) {
    cart->dir = (cart->dir - 1) % 4;
    return;
}

size_t move_straight(size_t start_index, struct minecart *cart) {
    switch (cart->dir) {
    case D_EAST:
        return start_index + 1;
    case D_WEST:
        return start_index - 1;
    case D_NORTH:
        return start_index - MAP_SIZE;
    case D_SOUTH:
        return start_index + MAP_SIZE;
    }
    printf("Error! Cart has unknown direction (%d)!\n", cart->dir);
    return 0;
}

size_t move_curve(size_t start_index, struct minecart *cart, char symbol) {
    if (cart->dir == D_NORTH || cart->dir == D_SOUTH) {
        if (symbol == S_NE_SW_CURVE) {
            turn_right(cart);
        } else {
            turn_left(cart);
        }
    } else {
        if (symbol == S_NE_SW_CURVE) {
            turn_left(cart);
        } else {
            turn_right(cart);
        }
    }
    return move_straight(start_index, cart);
}

size_t move_intersection(size_t start_index, struct minecart *cart) {
    switch (cart->next_turn) {
    case T_LEFT:
        turn_left(cart);
        break;
    case T_AHEAD:
        break;
    case T_RIGHT:
        turn_right(cart);
        break;
    }
    cart->next_turn = (cart->next_turn + 1) % 3;
    return move_straight(start_index, cart);
}

size_t load_map(char *filename, struct node *buffer) {
    /* Loads a 150x150 grid */
    /* MALLOCS MINECARTS */
    FILE *fp = fopen(filename, "r");
    size_t i = 0;
    size_t cart_count = 0;
    while (i < MAP_SIZE * MAP_SIZE) {
        char c = fgetc(fp);
        switch (c) {
        case S_EMPTY:
        case S_H_STRAIGHT:
        case S_V_STRAIGHT:
        case S_NE_SW_CURVE:
        case S_NW_SE_CURVE:
        case S_INTERSECTION:
            buffer[i].cart = (void *) 0;
            buffer[i++].symbol = c;
            break;
        case S_TRAIN_N:
        case S_TRAIN_S:
            buffer[i].cart = calloc(1, sizeof(struct minecart));
            (buffer[i].cart)->alive = true;
            (buffer[i].cart)->dir = (c == S_TRAIN_N) ? D_NORTH : D_SOUTH;
            (buffer[i].cart)->next_turn = T_LEFT;
            (buffer[i].cart)->age = 0;
            buffer[i++].symbol = S_V_STRAIGHT;
            cart_count++;
            break;
        case S_TRAIN_E:
        case S_TRAIN_W:
            buffer[i].cart = calloc(1, sizeof(struct minecart));
            (buffer[i].cart)->alive = true;
            (buffer[i].cart)->dir = (c == S_TRAIN_E) ? D_EAST : D_WEST;
            (buffer[i].cart)->next_turn = T_LEFT;
            (buffer[i].cart)->age = 0;
            buffer[i++].symbol = S_H_STRAIGHT;
            cart_count++;
            break;
        case '\n':
        default:
            break;
        }
    }
    fclose(fp);
    return cart_count;
}

size_t update(struct node *rails, uint64_t age) {
    /* Top-to-bottom, left-to-right */
    /* Returns whether a crash occured */
    size_t crash_count = 0;
    for (size_t row = 0; row < MAP_SIZE; row++) {
        for (size_t col = 0; col < MAP_SIZE; col++) {
            struct node *n = rails + (row * MAP_SIZE) + col;
            size_t target = (row * MAP_SIZE) + col;
            if (n->cart && ((n->cart)->alive) && ((n->cart)->age == age)) {
                switch (n->symbol) {
                case S_H_STRAIGHT:
                case S_V_STRAIGHT:
                    target = move_straight(target, n->cart);
                    break;
                case S_NE_SW_CURVE:
                case S_NW_SE_CURVE:
                    target = move_curve(target, n->cart, n->symbol);
                    break;
                case S_INTERSECTION:
                    target = move_intersection(target, n->cart);
                    break;
                case S_EMPTY:
                default:
                    printf("Error! Cart de-railed!\n");
                    printf("row=%lu, col=%lu, age=%lu\n", row, col, age);
                    crash_count++;
                }
                struct node *m = rails + target;
                if (m->cart) {
                    /* Crash! */
                    free(n->cart);
                    n->cart = (void *) 0;
                    (m->cart)->alive = false;
                    size_t crashrow = target / MAP_SIZE;
                    size_t crashcol = target % MAP_SIZE;
                    printf("Crashed at (%lu,%lu)!\n", crashcol, crashrow);
                    /* The elves quickly remove the other cart too */
                    free(m->cart);
                    m->cart = (void *) 0;
                    crash_count += 2;
                } else {
                    m->cart = n->cart;
                    n->cart = (void *) 0;
                    (m->cart)->age++;
                }
            }
        }
    }
    return crash_count;
}

void display_rails(struct node *rails) {
    for (int r = 0; r < MAP_SIZE; r++) {
        for (int c = 0; c < MAP_SIZE; c++) {
            struct node *n = rails + (MAP_SIZE * r) + c;
            if (n->cart) {
                printf("%c", ((n->cart)->alive) ? (">^<v")[(n->cart)->dir]
                                                : 'X');
            } else {
                printf("%c", n->symbol);
            }
        }
        printf("\n");
    }
    return;
}

void display_cart_coords(struct node *rails) {
    for (size_t row = 0; row < MAP_SIZE; row++) {
        for (size_t col = 0; col < MAP_SIZE; col++) {
            struct node *n = rails + (row * MAP_SIZE) + col;
            if (n->cart) {
                printf("(%lu,%lu)\n", col, row);
                return;
            }
        }
    }
    printf("Error: no carts remaining on track.\n");
    return;
}

int main(void) {
    struct node *rails = calloc(MAP_SIZE * MAP_SIZE, sizeof(struct node));
    size_t cart_count = load_map("input13.txt", rails);
    uint64_t age = 0;
    while (cart_count > 1) {
        size_t crash_count = update(rails, age);
        age++;
        if (crash_count) {
            cart_count -= crash_count;
            printf("%lu cart(s) left\n", cart_count);
        }
        /*display_rails(rails);*/
    }
    printf("Final cart at ");
    display_cart_coords(rails);
    return 0;
}
