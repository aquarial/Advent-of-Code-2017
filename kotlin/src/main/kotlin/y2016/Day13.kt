package y2016


import java.util.*
import kotlin.system.exitProcess

fun main(args: Array<String>) {
//    val num = readLine()!!.toInt()
    val num = 1362


    fun m(x: Int, y: Int): Boolean {
        val math = x * x + 3 * x + 2 * x * y + y + y * y + num
        val count = Integer.toBinaryString(math).count { it == '1' }
        return count % 2 == 0
    }

    val maze = MutableList(50, { y -> MutableList(50, { x -> m(x, y) }) })

    data class Pos(val x: Int, val y: Int) {
        var depth = 0
    }

    val visited = mutableSetOf<Pos>()
    val toVisit = PriorityQueue<Pos>(kotlin.Comparator { o1, o2 -> o1.depth.compareTo(o2.depth) })
    toVisit.add(Pos(1, 1))

    while (toVisit.isNotEmpty()) {
        val current = toVisit.remove()

        if (visited.contains(current))
            continue

        if (current == Pos(31, 39)) {
            println(current.depth)
        }


        visited.add(current)

        val x = current.x
        val y = current.y

        val deltas = listOf(Pair(1, 0), Pair(0, 1), Pair(-1, 0), Pair(0, -1))

        deltas
                .filter { x + it.first in maze.indices }
                .filter { y + it.second in maze[0].indices }
                .filter { maze[y + it.second][x + it.first] }
                .mapTo(toVisit) {
                    val c = current.copy(x = x + it.first, y = y + it.second)
                    c.depth = current.depth + 1
                    c
                }

    }
//
//    for (y in 0..40) {
//        for (x in 0..40) {
//            if (x == 1 && y == 1) {
//                print(".")
//                continue
//            }
//            if (x == 31 && y == 39) {
//                print("Z")
//                continue
//            }
//
//            print(charHere(x, y))
//        }
//        println()
//    }

}