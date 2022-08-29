#include <malloc.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdint.h>

#define NUM_PROBES 1000
#define abs(x) ((x) < 0 ? -(x) : (x))
#define avg(x,y) ((x) + (y)) / 2

struct coord {
    int32_t x;
    int32_t y;
    int32_t z;
};

struct probe {
    struct coord centre;
    int32_t radius;
};

struct bounding_box {
    struct coord corner; /* minx, miny, minz */
    int32_t size;
};

struct bbqueue { /* lol */
    int32_t score;
    uint32_t dist;
    struct bounding_box box;
    struct bbqueue *next;
};

void read_probes(char *filename, struct probe *probes) {
    FILE *fp = fopen(filename, "r");
    int ret;
    do {
        ret = fscanf(fp, "pos=<%d,%d,%d>, r=%d\n",
            &(probes->centre.x), &(probes->centre.y), &(probes->centre.z),
            &(probes->radius));
        probes++;
    } while (ret != EOF);
    return;
}

size_t find_biggest_probe(struct probe *probes) {
    size_t biggest = 0;
    for (size_t i = 0; i < NUM_PROBES; i++) {
        if (probes[i].radius > probes[biggest].radius)
            biggest = i;
    }
    return biggest;
}

uint32_t manhattan(struct coord a, struct coord b) {
    int32_t dx = a.x - b.x;
    int32_t dy = a.y - b.y;
    int32_t dz = a.z - b.z;
    return abs(dx) + abs(dy) + abs(dz);
}

bool probe_contains_point(struct probe p, struct coord x) {
    return manhattan(p.centre, x) <= p.radius;
}

struct coord box_centre(struct bounding_box b) {
    return ((struct coord) {
        b.corner.x + (b.size / 2),
        b.corner.y + (b.size / 2),
        b.corner.z + (b.size / 2)
    });
}

bool box_intersects_probe(struct bounding_box b, struct probe p) {
    if (b.size == 1)
        return probe_contains_point(p, b.corner);
    return manhattan(box_centre(b), p.centre) <= (b.size * 2) + p.radius;
}

uint32_t count_intersections(struct bounding_box b, struct probe *probes) {
    uint32_t count = 0;
    for (size_t i = 0; i < NUM_PROBES; i++) {
        count += box_intersects_probe(b, probes[i]);
    }
    return count;
}

void print_queue(struct bbqueue *q) {
    for (size_t i = 0; i < 4; i++) {
        if (!q) {
            printf("*");
            break;
        }
        printf("(%u,%u,%d) - ", q->score, q->dist, q->box.size);
        q = q->next;
    }
    printf("\n");
    return;
}

/* World's worst priority queue */
void enqueue (
    struct bbqueue **q,
    struct bounding_box b,
    uint32_t score,
    uint32_t dist
) {
    /*
    printf("Enqueuing <%d,%d,%d>"
        "score=%u, dist=%u, size=%d\n",
        b.corner.x, b.corner.y, b.corner.z,
        score, dist, b.size);
    */
    /* highest score is front of queue */
    /* if score is tied, break by size */
    /* if size is tied, break by dist */
    struct bbqueue *n = calloc(1, sizeof(struct bbqueue));
    n->box = b;
    n->score = score;
    n->dist = dist;
    n->next = (void *) 0;
    if (!(*q)) {
        *q = n;
    } else {
        /* There's a more elegant way to do this but meh */
        struct bbqueue *prev = (void *) 0;
        struct bbqueue *current;
        for (current = *q; current; current = current->next) {
            if (score > current->score)
                break;
            if (score == current->score
                && dist < current->dist)
                break;
            if (score == current->score
                && dist == current->dist
                && b.size < (current->box).size)
                break;
            prev = current;
        }
        /* Insert here! */
        n->next = current;
        if (prev) {
            prev->next = n;
        } else {
            *q = n;
        }
    }
    /*
    print_queue(*q);
    */
    return;
}

struct bounding_box dequeue (struct bbqueue **q) {
    struct bbqueue *tmp = *q;
    struct bounding_box b = tmp->box;
    (*q) = tmp->next;
    free(tmp);
    return b;
}

void free_all(struct bbqueue *q) {
    while (q) {
        struct bbqueue *tmp = q;
        q = q->next;
        free(tmp);
    }
    return;
}

struct bounding_box initialise_bounds(struct probe *probes) {
    int32_t minx = probes[0].centre.x,
            maxx = probes[0].centre.x,
            miny = probes[0].centre.y,
            maxy = probes[0].centre.y,
            minz = probes[0].centre.z,
            maxz = probes[0].centre.z;
    for (size_t i = 0; i < NUM_PROBES; i++) {
        if (probes[i].centre.x < minx)
            minx = probes[i].centre.x;
        if (probes[i].centre.x > maxx)
            maxx = probes[i].centre.x;
        if (probes[i].centre.y < miny)
            miny = probes[i].centre.y;
        if (probes[i].centre.y > maxy)
            maxy = probes[i].centre.y;
        if (probes[i].centre.z < minz)
            minz = probes[i].centre.z;
        if (probes[i].centre.z > maxz)
            maxz = probes[i].centre.z;
    }
    int32_t size = 1;
    while ((minx + size < maxx)
        || (miny + size < maxy)
        || (minz + size < maxz)
    ) {
        size *= 2;
    }
    return ((struct bounding_box) {(struct coord) {minx, miny, minz}, size});
}

struct coord most_intersections(struct probe *probes) {
    const struct coord origin = (struct coord) {0, 0, 0};
    struct bounding_box b = initialise_bounds(probes);
    struct bbqueue *todo = (void *) 0;
    enqueue(&todo, b, NUM_PROBES, manhattan(box_centre(b), origin));
    while (todo) {
        b = dequeue(&todo);
        if (b.size == 1) {
            free_all(todo);
            return b.corner;
        }
        int32_t s = b.size / 2;
        struct bounding_box candidates[8] = {
            (struct bounding_box) {
                (struct coord) {
                    b.corner.x, b.corner.y, b.corner.z },
                s },
            (struct bounding_box) {
                (struct coord) {
                    b.corner.x, b.corner.y, b.corner.z + s },
                s },
            (struct bounding_box) {
                (struct coord) {
                    b.corner.x, b.corner.y + s, b.corner.z },
                s },
            (struct bounding_box) {
                (struct coord) {
                    b.corner.x, b.corner.y + s, b.corner.z + s },
                s },
            (struct bounding_box) {
                (struct coord) {
                    b.corner.x + s, b.corner.y, b.corner.z },
                s },
            (struct bounding_box) {
                (struct coord) {
                    b.corner.x + s, b.corner.y, b.corner.z + s, },
                s },
            (struct bounding_box) {
                (struct coord) {
                    b.corner.x + s, b.corner.y + s, b.corner.z },
                s },
            (struct bounding_box) {
                (struct coord) {
                    b.corner.x + s, b.corner.y + s, b.corner.z + s },
                s }
        };
        /*
        printf("===\n");
        */
        for (size_t i = 0; i < 8; i++)
            enqueue(
                &todo,
                candidates[i],
                count_intersections(candidates[i], probes),
                manhattan(origin, box_centre(candidates[i]))
            );
    }
    /* Should never happen */
    printf("UH OH!\n");
    return b.corner;
}

int main(void) {
    struct probe probes[NUM_PROBES] = {0};
    read_probes("input23.txt", probes);
    /* Part 1 */
    size_t biggest = find_biggest_probe(probes);
    uint32_t total = 0;
    for (size_t i = 0; i < NUM_PROBES; i++) {
        /* The probe includes itself */
        total += probe_contains_point(probes[biggest], probes[i].centre);
    }
    printf("%u\n", total);
    /* Part 2 */
    struct coord most = most_intersections(probes);
    /*
    printf("(%d,%d,%d)\n", most.x, most.y, most.z);
    total = 0;
    for (size_t i = 0; i < NUM_PROBES; i++) {
        total += probe_contains_point(probes[i], most);
    }
    printf("Intersects with %u probes\n", total);
    */
    printf("%u\n", manhattan(most, (struct coord) {0, 0, 0}));
    return 0;
}
