#include <stdio.h>
#include <stdint.h>

/* Compile with cc -Wall -O3 <this> */

int do_program(int64_t init) {
    int64_t r[6] = {init, 0, 0, 0, 0, 0};
    int counter = 0;
    r[1] = 0;
label_C:
    r[4] = r[1] | 65536;
    r[1] = 16298264;
label_F:
    r[5] = r[4] & 255;
    r[1] = r[1] + r[5];
    r[1] = r[1] & 16777215;
    r[1] = r[1] * 65899;
    r[1] = r[1] & 16777215;
    r[5] = 256 > r[4];
    if (r[5]) goto label_A;
/* label_B: */
    r[5] = 0;
label_D:
    r[3] = r[5] + 1;
    r[3] = r[3] * 256;
    r[3] = r[3] > r[4];
    if (r[3]) goto label_E;
    r[5] = r[5] + 1;
    goto label_D;
label_E:
    r[4] = r[5];
    goto label_F;
label_A:
    /*printf("%ld\n", r[1]);*/
    if (r[0] == r[1]) return counter;
    counter++;
    goto label_C;
}

int main(void) {
    int64_t init;
    printf("Enter a value for register 0.\n");
    scanf("%ld", &init);
    int res = do_program(init);
    printf("This number was reached after %d others.\n", res);
}
