#!/usr/bin/env scala
import scala.io.Source
import scala.collection.mutable

def swapEquipment(terrain: Int, currentEquipment: Int): Int = {
    // 0 = rocky  0 = neither
    // 1 = wet    1 = torch
    // 2 = narrow 2 = climbing gear
    3 - (terrain + currentEquipment)
}

def erosionLevel(
    depth: Int,
    target: List[Int],
    x: Int, y: Int,
    cache: mutable.Map[(Int,Int),Int]
): Int = {
    // Assumes depth, target never change
    lazy val geologic = (x, y) match {
        case ((target(0)), (target(1))) => 0
        case (x, 0) => 16807 * x
        case (0, y) => 48271 * y
        case (x, y) => erosionLevel(depth, target, x - 1, y, cache
                   ) * erosionLevel(depth, target, x, y - 1, cache)
    }
    cache.getOrElseUpdate((x, y), (geologic + depth) % 20183)
}

def rescueTarget(
    depth: Int,
    target: List[Int],
    cache: mutable.Map[(Int,Int),Int]
): Int = {
    // valid moves:
    // up (if upper terrain != equipment), similarly down, left, right => cost 1
    // change equipment (only one valid choice) => cost 7
    // nodes have (distance, x, y, equipment)
    val shortestDistance = mutable.Map.empty[(Int,Int,Int),Int]
    var toExplore = mutable.PriorityQueue.empty[(Int, Int, Int, Int)](
        Ordering.by((_: (Int, Int, Int, Int))._1).reverse
    )
    toExplore.enqueue((0, 0, 0, 1)) // top-left corner with torch
    while (toExplore.length > 0) {
        val (distance, x, y, equipment) = toExplore.dequeue()
        if (!shortestDistance.contains((x, y, equipment))) {
            //println(distance, x, y, equipment)
            shortestDistance.update((x, y, equipment), distance)
            if (target(0) == x && target(1) == y && equipment == 1) {
                // Note -- must have torch equipped to finish search
                return distance
            }
            val opts: List[(Int,Int,Int,Int)] = List(
                (distance + 1, x + 1, y, equipment),
                (distance + 1, x - 1, y, equipment),
                (distance + 1, x, y + 1, equipment),
                (distance + 1, x, y - 1, equipment),
                (distance + 7, x, y, swapEquipment(
                    erosionLevel(depth, target, x, y, cache) % 3,
                    equipment))
            ).filter{
                case (td, tx, ty, te) => (
                    tx >= 0 && ty >= 0
                    && te != (erosionLevel(depth, target, tx, ty, cache) % 3)
                )
            }
            toExplore = toExplore ++ opts;
        }
    }
    return -1 // no path
}

def main() = {
    val lines = Source.fromFile("input22.txt")
                      .getLines()
                      .map(_.split(" ")(1))
    val depth = lines.next().toInt
    val target = lines.next().split(",").map(_.toInt).toList
    val cache = mutable.Map.empty[(Int,Int),Int]
    var total = 0;
    (0 to target(0)).map { x =>
        (0 to target(1)).map { y =>
            // probably not thread-safe, lol
            total += (erosionLevel(depth, target, x, y, cache) % 3)
        }
    }
    println(total)
    // Takes ~30s
    println(rescueTarget(depth, target, cache))
}

main()
