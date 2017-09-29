package y2015

fun main(args: Array<String>) {
    data class Pos(val x: Int, val y: Int)

    fun move(pos: Pos, c: Char): Pos = when (c) {
        '^' -> pos.copy(y = pos.y + 1)
        'v' -> pos.copy(y = pos.y - 1)
        '>' -> pos.copy(x = pos.x + 1)
        '<' -> pos.copy(x = pos.x - 1)
        else -> pos
    }

    val input = readLine()!!

    val houses = HashMap<Pos, Int>()
    var currPos = Pos(0, 0)

    houses[currPos] = 1
    var count = 1

    for (c in input) {
        currPos = move(currPos, c)
        if (houses[currPos] == null)
            count++
        houses.put(currPos, houses.getOrDefault(currPos, 0) + 1)

        println(count)
    }
}