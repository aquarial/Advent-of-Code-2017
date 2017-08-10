fun main(args: Array<String>) {
    val buttons = arrayOf(
            "  1  ",
            " 234 ",
            "56789",
            " ABC ",
            "  D  "
    )


    data class Pos(var x: Int, var y: Int)

    fun getButton(p: Pos) = buttons[p.y][p.x]
    fun invalid(p: Pos) = getButton(p) == ' '
    fun move(p: Pos, d: Char): Pos {
        val newP = when (d) {
            'U' -> if (p.y == 0) p.copy() else p.copy(y = p.y - 1)
            'D' -> if (p.y == 4) p.copy() else p.copy(y = p.y + 1)
            'L' -> if (p.x == 0) p.copy() else p.copy(x = p.x - 1)
            'R' -> if (p.x == 4) p.copy() else p.copy(x = p.x + 1)
            else -> p.copy()
        }
        return if (invalid(newP)) p.copy() else newP
    }

    // not 5685
    var p = Pos(0, 2)
    while (true) {
        val line = readLine()!!

        for (c in line) {
            p = move(p, c)
        }
        println(getButton(p))
    }
}