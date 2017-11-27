package y2016


fun main(args: Array<String>) {
    val buttons = arrayOf(
            "       ",
            "   1   ",
            "  234  ",
            " 56789 ",
            "  ABC  ",
            "   D   ",
            "       "
    )
    data class Pos(var x: Int, var y: Int)
    fun getButton(p: Pos) = buttons[p.y][p.x]

    fun move(p: Pos, d: Char): Pos {
        val newP = when (d) {
            'U' -> p.copy(y = p.y - 1)
            'D' -> p.copy(y = p.y + 1)
            'L' -> p.copy(x = p.x - 1)
            'R' -> p.copy(x = p.x + 1)
            else -> p.copy()
        }
        return if (getButton(newP) == ' ') p.copy() else newP
    }

    // not 5685
    var p = Pos(1, 3)
    while (true) {
        val line = readLine()!!

        for (c in line) {
            p = move(p, c)
        }
        println(getButton(p))
    }
}