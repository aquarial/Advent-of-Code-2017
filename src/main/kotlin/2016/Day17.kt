import kotlin.system.exitProcess

fun main(args: Array<String>) {

    val stem = "pgflpeqp"

    data class Pos(val x: Int, val y: Int)
    data class State(val p: Pos, private val history: String) {

        fun isWin(): Boolean = p.x == 3 && p.y == 3

        fun isValid(): Boolean = p.x in 0..3 && p.y in 0..3 && !isWin()

        fun move(p: Pos, dir: Char): Pos = when (dir) {
            'U' -> p.copy(y = p.y - 1)
            'D' -> p.copy(y = p.y + 1)
            'L' -> p.copy(x = p.x - 1)
            'R' -> p.copy(x = p.x + 1)
            else -> p.copy()
        }

        fun openDoors(): List<Char> {
            return md5(stem + history)
                    .take(4)
                    .map { it in "bcdef" }
                    .zip("UDLR".asIterable())
                    .filter { it.first }
                    .map { it.second }
        }

        fun nextStates(): List<State> = openDoors().map { State(move(p, it), history + it) }
    }

    val states = mutableListOf(State(Pos(0, 0), ""))
    val nextStates = mutableSetOf<State>()

    var count = 0
    while (true) {

        for (s in states) {
//            if (s.isWin()) {
//                println(s)
//                exitProcess(0)
//            }
            s.nextStates()
                    .filter { it.isValid() }
                    .forEach { nextStates.add(it) }
        }

        count++
        println("After $count, there are ${nextStates.size} possible states")

        if (states.size == 0)
            exitProcess(1)

        states.clear()
        states.addAll(nextStates)
        nextStates.clear()
    }
}

// was 596??? not 600