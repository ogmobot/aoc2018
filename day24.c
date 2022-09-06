#define _GNU_SOURCE
#include <malloc.h>
#include <regex.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define BUFFER_LEN 1024
#define MAX_AD_TYPES 16
#define MAX_UNITS 32

struct unit {
    uint8_t team;
    uint8_t attack_type;
    bool valid_target;
    int32_t count;
    int32_t hp;
    uint32_t weakness;
    uint32_t immunity;
    int32_t attack_damage;
    int32_t initiative;
    struct unit *target;
};

/*** parsing ***/

char g_ad_table[MAX_AD_TYPES][BUFFER_LEN];

uint8_t ad_lookup(char *ad_name) {
    for (size_t i = 0; i < MAX_AD_TYPES; i++) {
        if (!strncmp(g_ad_table[i], ad_name, BUFFER_LEN))
            return i;
        if (g_ad_table[i][0] == '\0') {
            strncpy(g_ad_table[i], ad_name, BUFFER_LEN);
            return i;
        }
    }
    /* Not present in table, and no room for more */
    return UINT8_MAX;
}

void parse_weakness(char *text, uint32_t *weakness, uint32_t *immunity) {
    /*printf("Parsing \"%s\"\n", text);*/
    /* also immunities */
    regex_t matcher;
    regmatch_t matches[4];
    char buffer[BUFFER_LEN];
    /* match weakness */
    (*weakness) = 0;
    regcomp(
        &matcher,
        "weak to \\([a-z]\\+\\)\\(, \\([a-z]\\+\\)\\)\\?",
        0
    );
    int res = regexec(&matcher, text, 4, matches, 0);
    regfree(&matcher);
    if (!res) {
        size_t w1_len = matches[1].rm_eo - matches[1].rm_so;
        size_t w2_len = matches[3].rm_eo - matches[3].rm_so;
        memcpy(buffer, text + matches[1].rm_so, w1_len);
        buffer[w1_len] = '\0';
        (*weakness) |= (1 << ad_lookup(buffer));
        if (w2_len) {
            memcpy(buffer, text + matches[3].rm_so, w2_len);
            buffer[w2_len] = '\0';
            (*weakness) |= (1 << ad_lookup(buffer));
        }
    }
    /*printf("Weakness 0x%x\n", *weakness);*/
    /* match immunities */
    (*immunity) = 0;
    regcomp(
        &matcher,
        "immune to \\([a-z]\\+\\)",
        0
    );
    res = regexec(&matcher, text, 4, matches, 0);
    regfree(&matcher);
    if (!res) {
        size_t i_len = matches[1].rm_eo - matches[1].rm_so;
        memcpy(buffer, text + matches[1].rm_so, i_len);
        buffer[i_len] = '\0';
        (*immunity) |= (1 << ad_lookup(buffer));
    }
    /*printf("Immunity 0x%x\n", *immunity);*/
    return;
}

void init_unit(char *buffer, struct unit *u) {
    char weakness_immunity[BUFFER_LEN];
    size_t wi_len;
    char attack_type[BUFFER_LEN];
    size_t at_len;
    regex_t matcher;
    regcomp(
        &matcher,
        "\\([0-9]\\+\\) units each with \\([0-9]\\+\\) hit points "
        "\\(([^)]*) \\)\\?"
        "with an attack that does \\([0-9]\\+\\) \\([a-z]\\+\\) damage "
        "at initiative \\([0-9]\\+\\)",
        0
    );
    regmatch_t matches[7];
    int res = regexec(&matcher, buffer, 7, matches, 0);
    if (res == REG_NOMATCH) {
        printf("Error reading line:\n");
        printf("%s\n", buffer);
        return;
    }
    regfree(&matcher);
    /* Number of units */
    sscanf(buffer + matches[1].rm_so, "%d", &(u->count));
    /* Hit points per unit */
    sscanf(buffer + matches[2].rm_so, "%d", &(u->hp));
    /* Weaknesses and immunities */
    wi_len = matches[3].rm_eo - matches[3].rm_so;
    if (wi_len) {
        memcpy(weakness_immunity, buffer + matches[3].rm_so, wi_len);
        weakness_immunity[wi_len] = '\0';
        parse_weakness(weakness_immunity, &(u->weakness), &(u->immunity));
    }
    /* Attack damage */
    sscanf(buffer + matches[4].rm_so, "%d", &(u->attack_damage));
    /* Attack type */
    at_len = matches[5].rm_eo - matches[5].rm_so;
    memcpy(attack_type, buffer + matches[5].rm_so, at_len);
    attack_type[at_len] = '\0';
    u->attack_type = ad_lookup(attack_type);
    /*
    printf("Attack type: %s (%hu)\n", attack_type, u->attack_type);
    */
    /* Initiative */
    sscanf(buffer + matches[6].rm_so, "%d", &(u->initiative));
    return;
}

size_t read_all_units(char *filename, struct unit *units) {
    size_t n_units = 0;
    char *line = (void *) 0;
    size_t len = 0;
    FILE *fp = fopen(filename, "r");
    uint8_t current_team;
    int res = getline(&line, &len, fp);
    while (res != -1) {
        len = strnlen(line, len);
        if (line[len - 2] == ':') {
            /* Team name */
            if (!strncmp(line, "Immune System:\n", len)) {
                current_team = 0;
            } else {
                current_team = 1;
            }
        } else if (line[0] >= '0' && line[0] <= '9') {
            /* Unit */
            init_unit(line, units);
            units->team = current_team;
            units++;
            n_units++;
        }
        res = getline(&line, &len, fp);
    }
    free(line);
    fclose(fp);
    return n_units;
}

/*** mechanics ***/

int32_t find_damage(struct unit attacker, struct unit defender) {
    /* Assumes the targets aren't on the same team! */
    if ((1 << attacker.attack_type) & defender.immunity)
        return 0;
    if ((1 << attacker.attack_type) & defender.weakness)
        return attacker.count * attacker.attack_damage * 2;
    return attacker.count * attacker.attack_damage;
}

/*** comparision functions for sorting ***/

int compare_initiative(const void *ip, const void *jp, void *arg) {
    /* Higher initiative goes first */
    struct unit *units = (struct unit *) arg;
    size_t *i = (size_t *) ip;
    size_t *j = (size_t *) jp;
    return units[*j].initiative - units[*i].initiative;
}
int compare_power(const void *ip, const void *jp, void *arg) {
    /* Higher effective power goes first */
    struct unit *units = (struct unit *) arg;
    size_t *i = (size_t *) ip;
    size_t *j = (size_t *) jp;
    int32_t a_power = units[*i].count * units[*i].attack_damage;
    int32_t b_power = units[*j].count * units[*j].attack_damage;
    if (a_power == b_power) {
        return (units[*j].initiative - units[*i].initiative);
    } else {
        return b_power - a_power;
    }
}

void print_unit(struct unit u) {
    printf("Team %s\n", u.team == 0 ? "Immune" : "Infection");
    printf("Count %d\n", u.count);
    printf("HP %d\n", u.hp);
    printf("weakness %d %d %d %d %d\n", !!(u.weakness & 1), !!(u.weakness & 2), !!(u.weakness & 4), !!(u.weakness & 8), !!(u.weakness & 16));
    printf("immunity %d %d %d %d %d\n", !!(u.immunity & 1), !!(u.immunity & 2), !!(u.immunity & 4), !!(u.immunity & 8), !!(u.immunity & 16));
    printf("attack %d\n", u.attack_type);
}

int32_t battle(struct unit *units, size_t n_units) {
    size_t init_order[MAX_UNITS] = {};
    size_t power_order[MAX_UNITS] = {};
    int32_t timeout = 50000; /* Assume we're in a loop if we take this long */
    for (size_t i = 0; i < n_units; i++) {
        init_order[i] = i;
        power_order[i] = i;
    }
    qsort_r(init_order, n_units, sizeof(size_t), compare_initiative, units);
    qsort_r(power_order, n_units, sizeof(size_t), compare_power, units);
    size_t winning_team;
    while (timeout--) {
        /* Reset target validity */
        for (size_t i = 0; i < n_units; i++) {
            units[i].valid_target = (units[i].count > 0);
            /*
            print_unit(units[i]);
            */
        }
        /* Target selection */
        qsort_r(power_order, n_units, sizeof(size_t), compare_power, units);
        for (size_t i = 0; i < n_units; i++) {
            struct unit *current = &(units[power_order[i]]);
            if (current->count <= 0)
                continue;
            int32_t best_damage = 0;
            int32_t best_power = 0;
            int32_t best_init = 0;
            for (size_t j = 0; j < n_units; j++) {
                if (!(units[j].valid_target))
                    continue;
                if (units[j].team == current->team)
                    continue;
                int32_t predicted_damage = find_damage(*current, units[j]);
                int32_t tmp_power = units[j].count * units[j].attack_damage;
                if (predicted_damage < best_damage)
                    continue;
                if ((predicted_damage == best_damage)
                    && (tmp_power < best_power))
                    continue;
                if ((predicted_damage == best_damage)
                    && (tmp_power == best_power)
                    && (units[j].initiative < best_init))
                    continue;
                /* target this unit */
                best_damage = predicted_damage;
                best_power = tmp_power;
                best_init = units[j].initiative;
                current->target = &(units[j]);
            }
            if (best_damage > 0) {
                /*
                printf("[%lu] chose target [%lu] -- could deal %d\n",
                    power_order[i],
                    (current->target - units),
                    best_damage);
                */
                (current->target)->valid_target = false;
            } else {
                /*
                printf("[%lu] found no target\n", power_order[i]);
                */
                (current->target) = (void *) 0;
            }
        }
        /* Attacking */
        for (size_t i = 0; i < n_units; i++) {
            struct unit current = units[init_order[i]];
            if (current.count <= 0)
                continue;
            if (current.target == (void *) 0)
                continue;
            int32_t units_killed = find_damage(current, *(current.target))
                                   / (current.target->hp);
            ((current.target)->count) -= units_killed;
            /*
            printf("[%lu] killed %d unit(s)\n", init_order[i], units_killed);
            */
        }
        /* Cleanup */
        int32_t surviviors[2] = {0, 0};
        for (size_t i = 0; i < n_units; i++) {
            if (units[i].count > 0) {
                surviviors[units[i].team]++;
            } else {
                units[i].count = 0;
            }
        }
        if (surviviors[0] == 0 || surviviors[1] == 0) {
            winning_team = (surviviors[0] == 0);
            break;
        }
    }
    if (!timeout)
        return 0; /* draw by timeout */
    int32_t n_winners = 0;
    for (size_t i = 0; i < n_units; i++) {
        if (units[i].team == winning_team) {
            n_winners += units[i].count;
        }
    }
    return winning_team == 0 ? n_winners    /* immune */
                             : -n_winners;  /* infection */
}

void apply_boost(struct unit *units, size_t n, int32_t boost, uint8_t team) {
    for (size_t i = 0; i < n; i++) {
        if (units[i].team == team)
            units[i].attack_damage += boost;
    }
    return;
}

int main(void) {
    struct unit orig[MAX_UNITS] = {};
    struct unit units[MAX_UNITS] = {};
    size_t n_units = read_all_units("input24.txt", orig);
    int32_t res;
    /* part 1 */
    memcpy(units, orig, n_units * sizeof(struct unit));
    res = battle(units, n_units);
    printf("%d\n", res >= 0 ? res : -res);
    /* part 2 */
    int32_t boost = 1;
    do {
        memcpy(units, orig, n_units * sizeof(struct unit));
        apply_boost(units, n_units, boost, 0);
        res = battle(units, n_units);
        boost++;
    } while (res <= 0);
    printf("%d\n", res >= 0 ? res : -res);
    return 0;
}
