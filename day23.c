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

struct region {
    struct coord minx;
    struct coord maxx;
    struct coord miny;
    struct coord maxy;
    struct coord minz;
    struct coord maxz;
};

struct region probe2region (struct probe p) {
    struct region v;
    int32_t x = p.centre.x;
    int32_t y = p.centre.y;
    int32_t z = p.centre.z;
    int32_t r = p.radius;
    v.minx = (struct coord) {x - r, y, z};
    v.maxx = (struct coord) {x + r, y, z};
    v.miny = (struct coord) {x, y - r, z};
    v.maxy = (struct coord) {x, y + r, z};
    v.minz = (struct coord) {x, y, z - r};
    v.maxz = (struct coord) {x, y, z + r};
    return v;
}

struct probe region2probe (struct region r) {
    return (struct probe) {
        (struct coord) {
            avg(r.maxx.x, r.minx.x),
            avg(r.maxy.y, r.miny.y),
            avg(r.maxz.z, r.minz.z)
        },
        (r.maxx.x - r.minx.x) / 2
    };
}

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

/*
TODO
struct region overlap(struct region a, struct region b) {
    return (struct region) {0, 0, 0, 0, 0, 0};
}
*/

struct coord find_nearby(struct coord test_point, struct probe *probes) {
    uint32_t n_overlaps = 0;
    for (size_t j = 0; j < NUM_PROBES; j++) {
        n_overlaps += probe_contains_point(probes[j], test_point);
    }
    bool keep_going = true;
    while (keep_going) {
        keep_going = false;
        struct coord neighbours[26] = {
            {test_point.x - 1, test_point.y - 1, test_point.z - 1},
            {test_point.x - 1, test_point.y - 1, test_point.z + 0},
            {test_point.x - 1, test_point.y - 1, test_point.z + 1},
            {test_point.x - 1, test_point.y + 0, test_point.z - 1},
            {test_point.x - 1, test_point.y + 0, test_point.z + 0},
            {test_point.x - 1, test_point.y + 0, test_point.z + 1},
            {test_point.x - 1, test_point.y + 1, test_point.z - 1},
            {test_point.x - 1, test_point.y + 1, test_point.z + 0},
            {test_point.x - 1, test_point.y + 1, test_point.z + 1},
            {test_point.x + 0, test_point.y - 1, test_point.z - 1},
            {test_point.x + 0, test_point.y - 1, test_point.z + 0},
            {test_point.x + 0, test_point.y - 1, test_point.z + 1},
            {test_point.x + 0, test_point.y + 0, test_point.z - 1},
            /*{test_point.x + 0, test_point.y + 0, test_point.z + 0},*/
            {test_point.x + 0, test_point.y + 0, test_point.z + 1},
            {test_point.x + 0, test_point.y + 1, test_point.z - 1},
            {test_point.x + 0, test_point.y + 1, test_point.z + 0},
            {test_point.x + 0, test_point.y + 1, test_point.z + 1},
            {test_point.x + 1, test_point.y - 1, test_point.z - 1},
            {test_point.x + 1, test_point.y - 1, test_point.z + 0},
            {test_point.x + 1, test_point.y - 1, test_point.z + 1},
            {test_point.x + 1, test_point.y + 0, test_point.z - 1},
            {test_point.x + 1, test_point.y + 0, test_point.z + 0},
            {test_point.x + 1, test_point.y + 0, test_point.z + 1},
            {test_point.x + 1, test_point.y + 1, test_point.z - 1},
            {test_point.x + 1, test_point.y + 1, test_point.z + 0},
            {test_point.x + 1, test_point.y + 1, test_point.z + 1},
        };
        for (size_t i = 0; i < 26; i++) {
            uint32_t t_overlaps = 0;
            for (size_t j = 0; j < NUM_PROBES; j++) {
                t_overlaps += probe_contains_point(probes[j], neighbours[i]);
            }
            /*
            printf("t_overlaps=%d @ (%d, %d, %d) => manhattan=%u\n",
                t_overlaps,
                neighbours[i].x, neighbours[i].y, neighbours[i].z,
                manhattan((struct coord) {0, 0, 0}, neighbours[i]));
            */
            if (t_overlaps > n_overlaps) {
                keep_going = true;
                test_point = neighbours[i];
                n_overlaps = t_overlaps;
                continue;
            }
        }
    }
    return test_point;
}

int main(void) {
    struct probe probes[NUM_PROBES];
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
    /* The region with the most overlaps MUST get all of its vertices from
     * probes! */
    struct coord vertices[NUM_PROBES * 6];
    struct coord candidates[NUM_PROBES * 6];
    uint32_t most_overlaps = 0;
    size_t num_candidates = 0;
    for (size_t i = 0; i < NUM_PROBES * 6; i++) {
        if ((i % 6) == 0) {
            /*
            printf("===\n");
            */
            struct region tmp = probe2region(probes[i / 6]);
            vertices[i + 0] = tmp.minx;
            vertices[i + 1] = tmp.maxx;
            vertices[i + 2] = tmp.miny;
            vertices[i + 3] = tmp.maxy;
            vertices[i + 4] = tmp.minz;
            vertices[i + 5] = tmp.maxz;
        }
        /*
        printf("(%d, %d, %d)\n", vertices[i].x, vertices[i].y, vertices[i].z);
        */
        uint32_t overlaps = 0;
        for (size_t j = 0; j < NUM_PROBES; j++) {
            overlaps += probe_contains_point(probes[j], vertices[i]);
        }
        if (overlaps == 0) {
            printf("Something's screwy here!\n");
            printf("Coord (%d, %d, %d) has no overlaps",
                vertices[i].x, vertices[i].y, vertices[i].z);
            printf(" but should overlap with at least probe <%d, %d, %d> r=%d\n",
                probes[i/6].centre.x, probes[i/6].centre.y,
                probes[i/6].centre.z, probes[i/6].radius);
            printf("(line %lu)\n", i/6);
        }
        /*
        printf("%u\n", overlaps);
        */
        if (overlaps >= most_overlaps) {
            if (overlaps > most_overlaps) {
                printf("Index %lu: New best is %d\n", i, overlaps);
                num_candidates = 0;
                most_overlaps = overlaps;
            }
            /*
            printf("(added new candidate)\n");
            printf("(%d, %d, %d)\n", vertices[i].x, vertices[i].y, vertices[i].z);
            */
            candidates[num_candidates++] = vertices[i];
        }
    }
    printf("Most overlaps is %d\n", most_overlaps);
    for (size_t i = 0; i < num_candidates; i++) {
        printf("(%d,%d,%d)",
            candidates[i].x, candidates[i].y, candidates[i].z);
        printf(" => %u\n", manhattan(
            (struct coord) {0, 0, 0},
            candidates[i]
        ));
        struct coord nearby = find_nearby(candidates[i], probes);
        printf("... (%d,%d,%d)",
            candidates[i].x, candidates[i].y, candidates[i].z);
        printf(" => %u\n", manhattan(
            (struct coord) {0, 0, 0},
            nearby
        ));
    }
    return 0;
}

/* 33438815 is too low */
