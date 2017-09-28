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
    var currPosA = Pos(0, 0)
    var currPosB = Pos(0, 0)

    houses[currPosA] = 1
    var count = 1

    val iter = input.iterator()
    while (iter.hasNext()) {
        currPosA = move(currPosA, iter.nextChar())
        if (houses[currPosA] == null)
            count++
        houses.put(currPosA, houses.getOrDefault(currPosA, 0) + 1)


        currPosB = move(currPosB, iter.nextChar())
        if (houses[currPosB] == null)
            count++
        houses.put(currPosB, houses.getOrDefault(currPosB, 0) + 1)

        println(count)
    }
}