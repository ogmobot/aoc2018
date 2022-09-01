#!/usr/bin/env python3
import sys

def manhattan(a, b):
    return sum(map((lambda ax, bx: abs(ax - bx)), a, b))

def constellate(stars):
    # stars is a list of constellations
    if not stars:
        return []
    first, rest = stars[0], stars[1:]
    for i, candidate in enumerate(rest):
        if any(manhattan(a, b) <= 3 for a in first for b in candidate):
            return constellate(rest[:i] + rest[i + 1:] + [candidate + first])
    return [first] + constellate(rest)

def parse_star(s):
    return tuple([int(i) for i in s.split(",")])

def main(filename):
    with open(filename, "r") as fp:
        stars = [[parse_star(line)] for line in fp]
    print(len(constellate(stars)))

if __name__ == "__main__":
    sys.setrecursionlimit(3000) # lol
    main("input25.txt")
