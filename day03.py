#!/usr/bin/python3
from dataclasses import dataclass
FABRIC_SIZE = 1000

@dataclass
class Rectangle:
    id:     int
    x:      int
    y:      int
    width:  int
    height: int

    def overlaps(self, other):
        return (
            (self .x < (other.x + other.width )) and
            (other.x < (self .x + self .width )) and
            (self .y < (other.y + other.height)) and
            (other.y < (self .y + self .height))
        )

def getrect(text):
    #1 @ x,y: wxh
    text = "".join((c if c.isdigit() else " ") for c in text)
    vals = [int(word) for word in text.split()]
    return Rectangle(*vals)

def main():
    input_file = open("input03.txt")
    rects = [getrect(line) for line in input_file]

    # part 1
    fabric = [[0]*FABRIC_SIZE for i in range(FABRIC_SIZE)]
    for rect in rects:
        for x in range(rect.x, rect.x+rect.width):
            for y in range(rect.y, rect.y+rect.height):
                fabric[x][y] += 1
    print(
        sum(
            [(fabric[x][y] >= 2)
                for x in range(FABRIC_SIZE)
                for y in range(FABRIC_SIZE)]))
    # part 2
    for rect in rects:
        rect.chill = True
    for index, rect in enumerate(rects):
        for other in rects[index+1:]:
            if rect.overlaps(other):
                rect.chill = False
                other.chill = False
    print([r.id for r in rects if r.chill].pop())

if __name__=="__main__":
    main()
