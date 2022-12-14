#include <malloc.h>
#include <stdio.h>
#include <stdint.h>

/* Compile with cc -Wall -O3 <this> */

/* Expect this to grow to ~12543 elements. Hash set would be faster but meh. */
struct stack {
    int64_t value;
    struct stack *next;
};

void push_stack(struct stack **s, int64_t value) {
    struct stack *n = calloc(1, sizeof(struct stack));
    n->value = value;
    n->next = *s;
    *s = n;
    return;
}

void free_stack(struct stack *s) {
    if (s) {
        free_stack(s->next);
        free(s);
    }
    return;
}

int stack_contains(struct stack *s, int64_t value) {
    if (!s)
        return 0;
    if (s->value == value)
        return 1;
    return stack_contains(s->next, value);
}

/* Translation of assembly program */
int64_t do_program(int64_t init, int32_t mode) {
    /* Mode 0: run normally
     * Mode 1: solve part 1
     * Mode 2: solve part 2
     */
    struct stack *ns = (void *) 0;
    int64_t random_val = 0, salt, tmp;
    int64_t counter = 0;
    while (1) {
        salt = random_val | 0x10000;
        random_val = 16298264;
        /* Generate pseudorandom values */
        while (1) {
            random_val = (((random_val + (salt & 0xFF)) & 0xFFFFFF)
                * 65899) & 0xFFFFFF;
            if (salt < 256) break;
            for (tmp = 0; ((tmp + 1) * 0x100 <= salt); tmp++)
                ;
            salt = tmp;
        }
        /* Test whether pseudorandom value equals the given init value */
        /*printf("=> %8ld\n", r[1]);*/
        switch (mode) {
        case 0:
            if (init == random_val)
                return counter;
            break;
        case 1:
            return random_val;
        case 2:
            if (stack_contains(ns, random_val)) {
                int64_t res = ns->value;
                free_stack(ns);
                return res;
            }
            push_stack(&ns, random_val);
            break;
        }
        counter++;
    }
}

int main(void) {
    /*
    int64_t init;
    printf("Enter a value for register 0.\n");
    scanf("%ld", &init);
    int res = do_program(init);
    printf("This number was reached after %d others.\n", res);
    */
    printf("%ld\n", do_program(0, 1));
    printf("%ld\n", do_program(0, 2));
}
