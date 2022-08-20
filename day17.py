#!/usr/bin/env python3
from collections import defaultdict
from math import inf

class World:
    symbols = {
        "empty":    ".",
        "clay":     "#",
        "rapid":    "|",
        "water":    "~",
        "spring":   "+",
    }
    def __init__(self):
        self.data = defaultdict(lambda: World.symbols["empty"])
        self.data[(500, 0)] = World.symbols["spring"]
        self.shallowest_y = None
        self.deepest_y = None
        self.dripstack = []

    def add_line(self, text):
        first, last = text.split(", ")
        bounds = last[last.index("=") + 1:].split("..")
        if first.startswith("x="): # vertical line
            x = int(first.lstrip("x="))
            for y in range(int(bounds[0]), int(bounds[1]) + 1):
                self.data[(x, y)] = World.symbols["clay"]
            if self.shallowest_y != None:
                self.shallowest_y = min((self.shallowest_y, int(bounds[0])))
            else:
                self.shallowest_y = int(bounds[0])
            if self.deepest_y != None:
                self.deepest_y    = max((self.deepest_y, int(bounds[1])))
            else:
                self.deepest_y = int(bounds[1])
        elif first.startswith("y="): # horixontal line
            y = int(first.lstrip("y="))
            for x in range(int(bounds[0]), int(bounds[1]) + 1):
                self.data[(x, y)] = World.symbols["clay"]
            if self.shallowest_y != None and self.deepest_y != None:
                self.shallowest_y = min((self.shallowest_y, y))
                self.deepest_y    = max((self.deepest_y, y))
            else:
                self.shallowest_y = y
                self.deepest_y = y
        else:
            raise ValueError(f"couldn't parse {text}")

    def drip(self, start_coord):
        self.dripstack.append(start_coord)
        # fall down
        passthru = [
            World.symbols["rapid"],
            World.symbols["empty"],
        ]
        keep_going = True
        #timer = 0
        while keep_going and self.data[start_coord] in passthru:
            #timer += 1
            #if timer % 10 == 0:
                #print(self)
                #print()
            tx, ty = start_coord
            while self.data[(tx, ty)] in passthru:
                if ty > self.deepest_y:
                    return False
                self.data[(tx, ty)] = World.symbols["rapid"]
                ty += 1
            keep_going = self.spread_water((tx, ty - 1))
        self.dripstack.pop()
        if self.data[start_coord] not in passthru:
            self.drip(self.dripstack[-1])

    def spread_water(self, start_coord):
        passthru = [
            World.symbols["rapid"],
            World.symbols["empty"],
        ]
        # spread out
        drip_again = False
        wall_coords = []
        for dx in [-1, 1]:
            tx, ty = start_coord
            while self.data[(tx, ty)] in passthru:
                self.data[(tx, ty)] = World.symbols["rapid"]
                if self.data[(tx, ty + 1)] in passthru:
                    self.drip((tx, ty + 1))
                    break
                tx += dx
            else:
                wall_coords.append(tx)
        if len(wall_coords) == 2:
            for tx in range(wall_coords[0] + 1, wall_coords[1]):
                self.data[(tx, ty)] = World.symbols["water"]
            drip_again = True
        return drip_again

    def count_water(self):
        return len([
            val
            for key, val in self.data.items()
            if (
                (key[1] <= self.deepest_y
                    and key[1] >= self.shallowest_y)
                and (val == World.symbols["water"]
                    or val == World.symbols["rapid"])
            )
        ])
    def count_still(self):
        return len([
            val
            for val in self.data.values()
            if val == World.symbols["water"]
        ])

    def __str__(self):
        min_x = min(k[0] for k in self.data.keys())
        max_x = max(k[0] for k in self.data.keys())
        min_y = min(k[1] for k in self.data.keys())
        max_y = max(k[1] for k in self.data.keys())
        buffer = [
            "".join(
                self.data[(x, y)]
                for x in range(min_x, max_x + 1)
            )
            for y in range(min_y, max_y + 1)
        ]
        return "\n".join(buffer)

def main(filename):
    world = World()
    with open(filename, "r") as fp:
        for line in fp.readlines():
            world.add_line(line)
    world.drip((500, 1))
    #print(world)
    print(world.count_water())
    print(world.count_still())

if __name__ == "__main__":
    main("input17.txt")
