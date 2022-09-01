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
int do_program(int64_t init, int32_t mode) {
    /* Mode 0: run normally
     * Mode 1: solve part 1
     * Mode 2: solve part 2
     */
    struct stack *ns = (void *) 0;
    int64_t r[6] = {init, 0, 0, 0, 0, 0};
    int counter = 0;
label_reset:
    r[4] = r[1] | 0x10000;
    r[1] = 16298264;
label_loop:
    r[1] = (((r[1] + (r[4] & 0xFF)) & 0xFFFFFF) * 65899) & 0xFFFFFF;
    if (256 > r[4]) goto label_compare;
    for (r[5] = 0; !((r[5] + 1) * 0x100 > r[4]); r[5]++)
        ;
    r[4] = r[5];
    goto label_loop;
label_compare:
    /*printf("%ld\n", r[1]);*/
    switch (mode) {
    case 0:
        if (r[0] == r[1])
            return counter;
    case 1:
        return r[1];
    case 2:
        if (stack_contains(ns, r[1])) {
            int res = ns->value;
            free_stack(ns);
            return res;
        }
        push_stack(&ns, r[1]);
        break;
    }
    counter++;
    goto label_reset;
}

int main(void) {
    /*
    int64_t init;
    printf("Enter a value for register 0.\n");
    scanf("%ld", &init);
    int res = do_program(init);
    printf("This number was reached after %d others.\n", res);
    */
    printf("%d\n", do_program(0, 1));
    printf("%d\n", do_program(0, 2));
}
