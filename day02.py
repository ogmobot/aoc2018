# file created 2021-10-22

def is_double (word):
    return any([word.count(letter) == 2 for letter in word])

def is_triple (word):
    return any([word.count(letter) == 3 for letter in word])

def count_differences (a, b):
    return sum([a[index] != b[index] for index in range(len(a))])

input_file = open("input02.txt")
words = [line.strip() for line in input_file]

total_doubles = 0
total_triples = 0
for first_word in words:
    # part 1
    if is_double(first_word):
        total_doubles += 1
    if is_triple(first_word):
        total_triples += 1
    # part 2
    for second_word in words:
        if count_differences(first_word, second_word) == 1:
            print(first_word)

print(total_doubles * total_triples)
