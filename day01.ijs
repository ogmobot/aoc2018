numbers=: ".>cutopen toJ 1!:1<'input01.txt'

echo +/ numbers

lengthen=:,~^:10
firstdup=:{.@#~-.@~:

echo firstdup +/\ lengthen numbers

exit''
