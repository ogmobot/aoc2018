#!/usr/bin/python3

manhattan = lambda a, b: abs(a[0]-b[0]) + abs(a[1]-b[1])

def get_points(filename):
    with open(filename) as inputfile:
        return [
            (lambda x: (int(x[0]), int(x[1])))(line.strip().split(","))
            for line in inputfile]

def nearest_point(pair, points):
    results = sorted(points, key=(lambda v: manhattan(pair, v)))
    if manhattan(pair, results[0]) == manhattan(pair, results[1]):
        return None
    else:
        return results[0]

def part1(points):
    sizes_a = {}
    sizes_b = {}
    # python is slooow :(
    for x in range(-100, 300):
        for y in range(-100, 300):
            nearest = nearest_point((x, y), points)
            if nearest:
                sizes_a[nearest] = sizes_a.get(nearest, 0) + 1
    for x in range(-101, 301):
        for y in range(-101, 301):
            nearest = nearest_point((x, y), points)
            if nearest:
                sizes_b[nearest] = sizes_b.get(nearest, 0) + 1
    region_sizes = []
    for k, v in sizes_a.items():
        if v == sizes_b.get(k,-1):
            # assume this is the only way a region can be finite
            region_sizes.append(v)
    print(sorted(region_sizes).pop())
    return

def part2(points):
    # There are 50 points, so if we are a distance of 200 from each,
    # that's a total of 10000 units. Hence, we don't need to go beyond
    # 200 above or below max/min
    result = 0
    for x in range(-100, 500):
        for y in range(-100, 500):
            total = sum(manhattan((x, y), p) for p in points)
            result += (total < 10000)
    print(result)
    return

def main():
    points = get_points("input06.txt")
    part1(points) # Takes ~7s to run
    part2(points) # Also takes ~7s to run

if __name__=="__main__":
    main()
