# file created 2021-10-22

input_file = open("input01.txt")
numbers = [int(line) for line in input_file]

print(sum(numbers))

total = 0
index = 0
already_seen = set()
while True:
    total = total + numbers[index % len(numbers)]
    if total in already_seen:
        print(total)
        break
    else:
        already_seen.add(total)
    index = index + 1
