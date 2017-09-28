@Suppress("LoopToCallChain")
class Screen(val width: Int, val height: Int) {
    val screen = mutableListOf<MutableList<Boolean>>()

    init {
        for (i in 1..height) {
            val tmp_row = mutableListOf<Boolean>()
            for (x in 1..width)
                tmp_row.add(false)
            screen.add(tmp_row)
        }

    }

    fun get(x: Int, y: Int) = screen[y][x]
    fun set(x: Int, y: Int, newV: Boolean) {
        screen[y][x] = newV
    }

    fun countLights(): Int {
        var count = 0
        for (y0 in 0 until height) {
            for (x0 in 0 until width) {
                if (screen[y0][x0])
                    count++
            }
        }
        return count
    }

    fun rect(x: Int, y: Int) {
        for (y0 in 0 until y) {
            for (x0 in 0 until x) {
                screen[y0][x0] = true
            }
        }
    }

    fun rotateColumn(col: Int, reps: Int) {
        for (tmp in 1..reps) {
            val temp = get(col, height - 1)
            for (y in height - 1 downTo 1) {
                set(col, y, get(col, y - 1))
            }
            set(col, 0, temp)
        }
    }

    fun rotateRow(row: Int, reps: Int) {
        for (tmp in 1..reps) {
            val temp = get(width - 1, row)
            for (x in width - 1 downTo 1) {
                set(x, row, get(x - 1, row))
            }
            set(0, row, temp)
        }
    }

}

fun main(args: Array<String>) {
    val screen = Screen(50, 6)
//    val screen = Screen(7, 3)
    while (true) {
        val line = readLine()!!

        "rect (\\d*)x(\\d*)".toRegex().matchEntire(line)?.groupValues?.let {
            screen.rect(it[1].toInt(), it[2].toInt())
        }

        "rotate column x=(\\d*) by (\\d*)".toRegex().matchEntire(line)?.groupValues?.let {
            screen.rotateColumn(it[1].toInt(), it[2].toInt())
        }

        "rotate row y=(\\d*) by (\\d*)".toRegex().matchEntire(line)?.groupValues?.let {
            screen.rotateRow(it[1].toInt(), it[2].toInt())
        }

        for (row in screen.screen) {
            print(row.map { if (it) '#' else '.' })
            println()
        }
        println(screen.countLights())
        println()
        println()
    }
}
