#include <malloc.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#define MAX_UNITS 32
#define MAX_TERRAIN 1024
#define UNIT_HP 200
#define TEAM_ELF 'E'
#define TEAM_GOB 'G'

struct unit {
    struct unit *next; /* In initiative order */
    size_t location;
    int32_t hp;
    char team;
};

struct terrain {
    char at[MAX_TERRAIN];
    uint8_t width;
    int32_t gob_atk;
    int32_t elf_atk;
};

struct path_node {
    struct path_node *next;
    size_t value;
    int32_t dist;
};

struct battle_outcome {
    size_t score;
    bool elf_died;
};

void load_terrain(char *filename, struct terrain *terrain, struct unit *units) {
    size_t location = 0;
    terrain->width = 0;
    terrain->elf_atk = 0;
    terrain->gob_atk = 0;
    FILE *fp = fopen(filename, "r");
    for (int c = fgetc(fp); c != EOF; c = fgetc(fp), location++) {
        switch (c) {
        case '\n':
            if (!(terrain->width))
                terrain->width = location;
            location--;
            break;
        case '.':
        case '#':
            terrain->at[location] = (char) c;
            break;
        case TEAM_ELF:
        case TEAM_GOB:
            units->next = (void *) 0;
            units->location = location;
            units->hp = UNIT_HP;
            units->team = (char) c;
            units++;
            units->team = '\0'; /* Ensure existence of sentry */
            terrain->at[location] = '.';
            break;
        default:
            fprintf(stderr, "Read error: unrecognised character '%c'.\n", c);
            break;
        }
    }
    fclose(fp);
}

int i32_less_than(const void *a, const void *b) {
    return *((int32_t *) a) - *((int32_t *) b);
}

bool i32_array_contains(int32_t *array, int32_t element) {
    /* Assumes array terminates with -1 */
    for (; *array != -1; array++) {
        if (*array == element)
            return true;
    }
    return false;
}

size_t total_hp(struct unit *units) {
    size_t total = 0;
    for (; units->team != '\0'; units++) {
        if (units->hp > 0) {
            total += units->hp;
        }
    }
    return total;
}

struct unit *unit_at(struct unit *units, size_t location) {
    while (units->team != '\0'
           && (units->location != location || units->hp <= 0))
        units++;
    return units;
}

struct unit *get_initiative(struct unit *units) {
    /* Push units onto initiative stack backwards */
    size_t max_location = 0;
    for (struct unit *ptr = units; ptr->team != '\0'; ptr++) {
        if (ptr->location > max_location && ptr->hp > 0) {
            max_location = ptr->location;
        }
    }
    struct unit *initiative = (void *) 0;
    for (size_t i = 0; i <= max_location; i++) {
        struct unit *ptr = unit_at(units, max_location - i);
        if (ptr->team != '\0') {
            ptr->next = initiative;
            initiative = ptr;
        }
    }
    return initiative;
}

bool empty_space(struct terrain *terrain, struct unit *units, size_t location) {
    for (; units->team != '\0'; units++) {
        if ((units->location == location) && (units->hp > 0))
            return false;
    }
    return terrain->at[location] == '.';
}

size_t find_target_locations(
    struct terrain *terrain,
    struct unit *units,
    char except,
    int32_t *buffer
) {
    /* Returns the number of locations added to buffer */
    int32_t *orig = buffer;
    for (struct unit *u = units; u->team != '\0'; u++) {
        if ((u->team == except) || (u->hp <= 0))
            continue;
        /* above */
        if (empty_space(terrain, units, u->location - terrain->width))
            *buffer++ = u->location - terrain->width;
        /* left */
        if (empty_space(terrain, units, u->location - 1))
            *buffer++ = u->location - 1;
        /* right */
        if (empty_space(terrain, units, u->location + 1))
            *buffer++ = u->location + 1;
        /* below */
        if (empty_space(terrain, units, u->location + terrain->width))
            *buffer++ = u->location + terrain->width;
    }
    *buffer = -1;
    return buffer - orig;
}

void find_all_distances(
    struct terrain *terrain,
    struct unit *units,
    size_t from_loc,
    int32_t *target_locs,
    size_t n_locs,
    int32_t *distances
) {
    static int32_t tmp_map[MAX_TERRAIN];
    for (size_t i = 0; i < MAX_TERRAIN; i++)
        tmp_map[i] = -1;
    /* Path nodes are a circular queue. We keep a pointer to the queue's tail,
     * which keeps a pointer to the queue's head. */
    struct path_node *queue = calloc(1, sizeof(struct path_node));
    queue->value = from_loc;
    queue->next = queue;
    queue->dist = 0;
    while (queue) {
        /* Pop from queue */
        struct path_node *current = queue->next;
        if (queue->next == queue) {
            queue = (void *) 0;
        } else {
            queue->next = queue->next->next;
        }
        /* Floodfill */
        if (tmp_map[current->value] == -1) {
            tmp_map[current->value] = current->dist;
            /* above */
            if (empty_space(terrain, units, current->value - terrain->width)) {
                struct path_node *tmp = calloc(1, sizeof(struct path_node));
                tmp->value = current->value - terrain->width;
                tmp->next = queue ? queue->next : tmp;
                tmp->dist = current->dist + 1;
                if (queue)
                    queue->next = tmp;
                queue = tmp;
            }
            /* left */
            if (empty_space(terrain, units, current->value - 1)) {
                struct path_node *tmp = calloc(1, sizeof(struct path_node));
                tmp->value = current->value - 1;
                tmp->next = queue ? queue->next : tmp;
                tmp->dist = current->dist + 1;
                if (queue)
                    queue->next = tmp;
                queue = tmp;
            }
            /* right */
            if (empty_space(terrain, units, current->value + 1)) {
                struct path_node *tmp = calloc(1, sizeof(struct path_node));
                tmp->value = current->value + 1;
                tmp->next = queue ? queue->next : tmp;
                tmp->dist = current->dist + 1;
                if (queue)
                    queue->next = tmp;
                queue = tmp;
            }
            /* below */
            if (empty_space(terrain, units, current->value + terrain->width)) {
                struct path_node *tmp = calloc(1, sizeof(struct path_node));
                tmp->value = current->value + terrain->width;
                tmp->next = queue ? queue->next : tmp;
                tmp->dist = current->dist + 1;
                if (queue)
                    queue->next = tmp;
                queue = tmp;
            }
        }
        free(current);
    }
    for (size_t i = 0; i < n_locs; i++) {
        distances[i] = tmp_map[target_locs[i]];
    }
    return;
}

size_t find_nearest_of(
    struct terrain *terrain,
    struct unit *units,
    int32_t *options,
    size_t n_options,
    size_t origin
) {
    /* Given a list of options (length n_options),
     * returns the one closest to origin */
    int32_t *distances = calloc(n_options, sizeof(int32_t));
    find_all_distances(
        terrain, units, origin, options, n_options, distances);
    size_t nearest_location = 0;
    int32_t best_distance = INT32_MAX;
    for (size_t i = 0; i < n_options; i++) {
        if (distances[i] < best_distance && distances[i] != -1) {
            nearest_location = options[i];
            best_distance = distances[i];
        }
    }
    /*
    for (size_t i = 0; i < n_options; i++) {
        printf("location %d -> distance %d\n", options[i], distances[i]);
    }
    */
    free(distances);
    return nearest_location;
}

struct unit *get_adjacent_enemy(
    struct terrain *terrain,
    struct unit *units,
    struct unit *active
) {
    /* Returns pointer to the adjacent enemy with least hp, or first in
     * reading order if tied. Returns 0 if no adjacent targets. */
    struct unit *adjacent_units[] = {
        unit_at(units, active->location - terrain->width),
        unit_at(units, active->location - 1),
        unit_at(units, active->location + 1),
        unit_at(units, active->location + terrain->width)
    };
    int32_t lowest_hp = UNIT_HP + 1;
    struct unit *target_unit = (void *) 0;
    for (size_t i = 0; i < 4; i++) {
        if (adjacent_units[i]
            && adjacent_units[i]->team != '\0'
            && adjacent_units[i]->team != active->team
            && adjacent_units[i]->hp < lowest_hp
        ) {
            target_unit = adjacent_units[i];
            lowest_hp = adjacent_units[i]->hp;
        }
    }
    return target_unit;
}

void print_unit(struct unit *);

bool step_unit(
    struct terrain *terrain,
    struct unit *units,
    struct unit *active
) {
#ifdef DEBUG_STEP
    printf("Stepping unit: ");
    print_unit(active);
#endif
    if (active->hp <= 0)
        return true;
    /* Check whether enemies exist */
    bool enemy_exists = false;
    for (struct unit *tmp = units; tmp->team != '\0'; tmp++) {
        if (tmp->hp > 0 && tmp->team != active->team) {
            enemy_exists = true;
        }
    }
    if (!enemy_exists)
        return false; /* No enemies found -- end turn early */
    /* If not already adjacent, move */
    if (!get_adjacent_enemy(terrain, units, active)) {
        /* Find empty spaces adjacent to enemies */
        int32_t in_range_locations[MAX_UNITS * 4];
        size_t len = find_target_locations(
            terrain, units, active->team, in_range_locations);
        /* Sort into reading order */
        qsort(in_range_locations, len, sizeof(int32_t), i32_less_than);
        /* Determine which one is closest */
        size_t target = (size_t) find_nearest_of(
            terrain, units, in_range_locations, len, active->location);
#ifdef DEBUG_STEP
        printf("  Target square: %lu\n", target);
#endif
        if (target != 0) {
            /* Determine the first step to take */
            int32_t first_steps[] = {
                active->location - terrain->width,
                active->location - 1,
                active->location + 1,
                active->location + terrain->width
            };
            size_t next_location = find_nearest_of(
                terrain, units, first_steps, 4, target);
            if (next_location != 0)
                active->location = next_location;
        }
    }
    /* If adjacent, attack */
    struct unit *target_unit = get_adjacent_enemy(terrain, units, active);
    if (target_unit) {
#ifdef DEBUG_STEP
        printf("  Attacking unit ");
        print_unit(target_unit);
#endif
        /* Attack it */
        target_unit->hp -= (active->team == TEAM_ELF ? terrain->elf_atk
                                                     : terrain->gob_atk);
    }
    return true; /* Unit ended turn normally */
}

bool step_battle(
    struct terrain *terrain,
    struct unit *units,
    struct unit *initiative
) {
    for (; initiative; initiative = initiative->next) {
        if (!step_unit(terrain, units, initiative))
            /* Unit ended turn early -- combat is over. */
            return false;
    }
    return true;
}

struct battle_outcome simulate_battle(
    struct terrain *terrain,
    struct unit *units
) {
    size_t round = 0;
    size_t initial_elf_count = 0;
    for (struct unit *u = units; u->team != '\0'; u++)
        initial_elf_count += ((u->team == TEAM_ELF) && (u->hp > 0));
    while (true) {
        struct unit *u = get_initiative(units);
#ifdef DEBUG_SHOW_MAP
        printf("After %2lu round(s):", round);
        draw_map(terrain, units);
        print_all_units(u);
#endif
        if (!step_battle(terrain, units, u))
            break;
        round++;
    }
#ifdef DEBUG_SHOW_MAP
    printf("Final state, after %lu round(s):", round);
    draw_map(terrain, units);
    print_all_units(get_initiative(units));
#endif
    size_t final_elf_count = 0;
    for (struct unit *u = units; u->team != '\0'; u++)
        final_elf_count += ((u->team == TEAM_ELF) && (u->hp > 0));
    return (struct battle_outcome) {
        round * total_hp(units),
        initial_elf_count > final_elf_count
    };
}

void draw_map(struct terrain *terrain, struct unit *units) {
    for (
        size_t location = 0;
        location < MAX_TERRAIN && terrain->at[location] != '\0';
        location++
    ) {
        if (location % terrain->width == 0)
            printf("\n%4lu ", location);
        struct unit *ptr = unit_at(units, location);
        printf("%c", ptr->team != '\0' ? ptr->team : terrain->at[location]);
    }
    printf("\n");
    return;
}

void print_unit(struct unit *u) {
    printf("%c(%lu) %d HP\n", u->team, u->location, u->hp);
    return;
}

void print_all_units(struct unit *initiative) {
    for (struct unit *u = initiative; u; u = u->next) {
        print_unit(u);
    }
    return;
}

int main(void) {
    struct terrain terrain = {};
    struct unit orig_units[MAX_UNITS] = {};
    load_terrain("input15.txt", &terrain, orig_units);
    terrain.elf_atk = 3;
    terrain.gob_atk = 3;
    struct unit units[MAX_UNITS] = {};
    memcpy(units, orig_units, MAX_UNITS * sizeof(struct unit));
    /* Part 1 */
    struct battle_outcome o = simulate_battle(&terrain, units);
    printf("%lu\n", o.score);
    /* Part 2 */
    while (o.elf_died) {
        terrain.elf_atk++;
        memcpy(units, orig_units, MAX_UNITS * sizeof(struct unit));
        o = simulate_battle(&terrain, units);
    }
    printf("%lu\n", o.score);
    return 0;
}
