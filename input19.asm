#ip 5           ;[0]=sum, [1]=divisor_upper, [2]=divisor_lower, [3]=n, [4]=tmp
seti 19 0 5     ;goto (FIRSTJUMP)
setr 3 0 1      ;EARLY: divisor_upper=n
seti 1 0 2      ;divisor_lower=1
mulr 1 2 4      ;LOOP: (a)           a               b          c
eqrr 4 3 4      ;  ... (b)           v               v          v
muli 4 6 4      ;  ... (c)
addr 4 5 5      ;  if (divisor_upper * divisor_lower == n) goto +7 (SUCCESS)
mulr 1 2 4      ;  (a)
gtrr 4 3 4      ;  (b)
muli 4 4 4      ;  (c)
addr 4 5 5      ;  if (divisor_upper * divisor_lower > n) goto +5 (NEXT)
addi 2 1 2      ;  divisor_lower++
seti 2 0 5      ;  goto LOOP
addr 0 1 0      ;  SUCCESS:
addr 0 2 0      ;  sum=sum+divisor_upper+divisor_lower
addi 1 -1 1     ;  NEXT: divisor_upper--
gtrr 2 1 4      ;  (a)
addr 5 4 5      ;  if (divisor_lower > divisor_upper) goto +2
seti 2 0 5      ;  goto LOOP
seti 999 0 5    ;HALT
seti 1025 0 3   ;FIRSTJUMP: n=1025
addr 5 0 5      ;if alpha goto +2
seti 0 0 5      ;goto 1 (EARLY)
seti 10551425 0 3 ; n=10551425
seti 0 0 0      ;sum=0
seti 0 0 5      ;goto 1 (EARLY)
