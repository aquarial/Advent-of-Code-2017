package y2016


import java.util.*
import kotlin.system.exitProcess

fun main(args: Array<String>) {

    val readInstructions = mutableListOf<String>()
    while (true) {
        val line = readLine() ?: break
        readInstructions.add(line)
    }


    var counter = 0
    while (true) {
        val start = "adcghbef".toMutableList()
        Collections.shuffle(start)
        val start_log = start.toList()

        fun swapIndices(a: Int, b: Int) {
            val tmp = start[a]
            start[a] = start[b]
            start[b] = tmp
        }

        fun rotate(direction: String, steps: Int) {
            val takeFrom = if (direction == "left") 0 else start.size - 1
            val putTo = if (direction == "left") start.size - 1 else 0

            for (i in 1..steps) {
                start.add(putTo, start.removeAt(takeFrom))
            }
        }


        for (line in readInstructions) {

            Regex("swap position (\\d+) with position (\\d+)").matchEntire(line)?.groupValues?.let {
                swapIndices(it[1].toInt(), it[2].toInt())
            }

            Regex("swap letter (\\w) with letter (\\w)").matchEntire(line)?.groupValues?.let {
                val a = start.indexOf(it[1][0])
                val b = start.indexOf(it[2][0])
                swapIndices(a, b)
            }

            Regex("rotate (\\w+) (\\d+) steps?").matchEntire(line)?.groupValues?.let {
                rotate(it[1], it[2].toInt())
            }

            Regex("rotate based on position of letter (\\w)").matchEntire(line)?.groupValues?.let {
                var steps = start.indexOf(it[1][0])
                if (steps >= 4) steps += 2 else steps++
                rotate("right", steps)
            }

            Regex("reverse positions (\\w) through (\\w)").matchEntire(line)?.groupValues?.let {
                val a = it[1].toInt()
                val b = it[2].toInt()
                val rev = start.subList(a, b + 1)
                rev.reverse()
            }

            Regex("move position (\\w) to position (\\w)").matchEntire(line)?.groupValues?.let {
                start.add(it[2].toInt(), start.removeAt(it[1].toInt()))
            }

        }


        if (counter++ % 10000 == 0)
            println("$start_log got $start")

        if (start == "fbgdceah".toList()) {
            println("ANSWER = " + start_log)
            exitProcess(0)
        }

    }

}

/*


swap position 4 with position 0
swap letter d with letter b
reverse positions 0 through 4
rotate left 1 step
move position 1 to position 4
move position 3 to position 0
rotate based on position of letter b
rotate based on position of letter d

 e  f  g  h  d  a  b  c
 e  f  h  g  d  a  b  c
[e, f, g, h, d, a, b, c]
 */