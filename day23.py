#!/usr/bin/env python3

from dataclasses import dataclass
import re
import sys
DEBUG = False

@dataclass
class Probe:
    x: int
    y: int
    z: int
    r: int
    def __hash__(self):
        return hash((self.x, self.y, self.z, self.r))

def load_probes(filename):
    probes = []
    with open(filename, "r") as fp:
        for line in fp:
            m = re.findall(r"pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(\d+)", line)
            probes.append(Probe(*[int(n) for n in m[0]]))
    return probes

def manhattan(a, b):
    return abs(a.x - b.x) + abs(a.y - b.y) + abs(a.z - b.z)

def a_contains_point_b(a: Probe, b) -> bool:
    return manhattan(a, b) <= a.r

def a_intersects_b(a: Probe, b: Probe) -> bool:
    return manhattan(a, b) <= a.r + b.r

def neighbour_set(all_probes: set[Probe], p: Probe) -> set[Probe]:
    return set(q for q in all_probes if a_intersects_b(p, q))

def biggest_cliques(
    partial: set[Probe],
    candidates: set[Probe],
    seen: set[Probe],
    result: list
):
    if len(candidates) == 0 and len(seen) == 0:
        if DEBUG: print(f"Found clique of size {len(partial)}")
        result.append(partial.copy())
        return
    for candidate in list(candidates):
        if candidate not in seen:
            biggest_cliques(
                partial.union({candidate}),
                set(c
                    for c in candidates
                    if c in neighbour_set(candidates, candidate) and c != candidate),
                set(x
                    for x in seen
                    if x in neighbour_set(candidates, candidate)),
                result
            )
            candidates.remove(candidate)
            seen.add(candidate)
    return result

def main():
    probes = load_probes("input23.txt")
    # part 1
    biggest = sorted(probes, key=(lambda p: p.r)).pop()
    n_contained = sum([a_contains_point_b(biggest, p) for p in probes])
    print(n_contained)
    # part 2
    result = []
    biggest_cliques(set(), set(probes), set(), result)
    biggest_clique = sorted(result, key=(lambda s: len(s))).pop()
    most_distant_probe = sorted(biggest_clique, key=(
        lambda p: manhattan(Probe(0, 0, 0, 0), p))).pop()
    print(manhattan(Probe(0, 0, 0, 0), most_distant_probe) - most_distant_probe.r)

if __name__ == "__main__":
    sys.setrecursionlimit(2000)
    main()
