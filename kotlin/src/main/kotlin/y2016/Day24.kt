@file:Suppress("LoopToCallChain")

package y2016

import java.util.*
import kotlin.system.exitProcess

fun main(args: Array<String>) {
    val input = mutableListOf<List<Char>>()
    while (true) {
        val line = readLine() ?: break
        input.add(line.toList())
    }


    data class Pos(val x: Int, val y: Int) {
        var depth = 0
    }


    val maze = input.toList()


    fun findChar(c: Char): Pos {
        for (y in 0 until maze.size) {
            for (x in 0 until maze[0].size) {
                if (maze[y][x] == c)
                    return Pos(x, y)
            }
        }
        println("Cound not find $c")
        exitProcess(0)
    }

    val targets = List(7 + 1, { findChar(it.toString()[0]) }).drop(1)


    var minSoFar = Int.MAX_VALUE
    while (true) {

        Collections.shuffle(targets)
        var steps = 0
        var lastTarget = findChar('0')

        targets@ for (t in targets) {
            val visited = mutableSetOf<Pos>()
            val toVisit = PriorityQueue<Pos>(Comparator { o1, o2 -> o1.depth.compareTo(o2.depth) })
            toVisit.add(lastTarget)

            while (toVisit.isNotEmpty()) {
                val current = toVisit.remove()

                if (visited.contains(current))
                    continue

                if (current == t) {
//                println("Found $t with ${current.depth} from $lastTarget")
                    steps += current.depth
                    lastTarget = t
                    continue@targets
                }


                visited.add(current)

                val x = current.x
                val y = current.y

                val deltas = listOf(Pair(1, 0), Pair(0, 1), Pair(-1, 0), Pair(0, -1))

                deltas
                        .filter { x + it.first in maze[0].indices }
                        .filter { y + it.second in maze.indices }
                        .filter { maze[y][x] != '#' }
                        .mapTo(toVisit) {
                            val c = current.copy(x = x + it.first, y = y + it.second)
                            c.depth = current.depth + 1
                            c
                        }

            }


        }


        val visited = mutableSetOf<Pos>()
        val toVisit = PriorityQueue<Pos>(Comparator { o1, o2 -> o1.depth.compareTo(o2.depth) })
        toVisit.add(lastTarget)

        while (toVisit.isNotEmpty()) {
            val current = toVisit.remove()

            if (visited.contains(current))
                continue

            if (current == findChar('0')) {
//                println("Found $t with ${current.depth} from $lastTarget")
                steps += current.depth
                lastTarget = findChar('0')
                break
            }


            visited.add(current)

            val x = current.x
            val y = current.y

            val deltas = listOf(Pair(1, 0), Pair(0, 1), Pair(-1, 0), Pair(0, -1))

            deltas
                    .filter { x + it.first in maze[0].indices }
                    .filter { y + it.second in maze.indices }
                    .filter { maze[y][x] != '#' }
                    .mapTo(toVisit) {
                        val c = current.copy(x = x + it.first, y = y + it.second)
                        c.depth = current.depth + 1
                        c
                    }
        }

//    println("$targets was $steps steps")
        val tmp = minSoFar
        minSoFar = Math.min(steps, minSoFar)
        if (tmp != minSoFar)
            println(minSoFar)
//    println("Min so far = $minSoFar")
//    println()
    }
}

// > 416
// < 464