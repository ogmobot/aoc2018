SIZE=: 300
k   =: 4842          NB. puzzle input
hun =: 10&|@<.@%&100 NB. hundreds digit
fn  =: [*+&k@*
pow =: (+&10@[)(-&5@hun@fn)]
pad =: 0,0,~0,.0,.~] NB. wrap with zeros -- taken from Rosetta Code
deltas =: ,@:(<@,"0/~&i.) NB. dx and dy for n-by-n squares

grid =: pad (pow"0/~(1+i. SIZE)) NB. pad with zeros so we can 1-index it

NB. echo grid NB. transposed compared to website

NB. find top-lefts of squares with 1+&.> deltas n
coords =. 1+&.> deltas (SIZE-2)

NB. part 1
echo {. coords \: (+/ grid {~ (deltas 3) +&.>/coords)

NB. part 2
all_deltas =: deltas &.> (1+i. SIZE)

NB. (+/@:{&grid)&.> all_deltas

NB. TODO write a function that accepts a square size n, and finds the
NB.      highest-value n-by-n square in the grid.
NB.      i.e. the below, but properly

NB. time to write FORTRAN in any language.
3 : 0''
best =. 0
best_coord =. (0 0 0)
for_n. 1 + i. SIZE do. NB. actually only need to go to 10
    the_deltas =. deltas n
    for_upper. 1 + i. (SIZE - n) do.
        for_left. 1 + i. (SIZE - n) do.
            subtotal =. +/ grid {~ (upper,left)&+&.> the_deltas
            if. subtotal > best do.
                best =. subtotal
                best_coord =. (upper, left, n)
                NB. echo best ; best_coord
            end.
        end.
    end.
end.
echo best ; best_coord
)

