import kotlin.system.exitProcess

data class Pos(var x: Int, var y: Int)

fun main(args: Array<String>) {
    val line = readLine()!!
    val instructions = line.split(", ")


    var dir = 0
    var pos = Pos(0, 0)

    val places = mutableSetOf<Pos>()
    places.add(pos)

    for (instr in instructions) {

        val turn = instr[0]
        val num = instr.substring(1).toInt()

        when (turn) {
            'L' -> dir = (dir + 3) % 4
            'R' -> dir = (dir + 1) % 4
        }

        for (i in 1..num) {
            when (dir) {
                0 -> pos = pos.copy(y = pos.y + 1)
                1 -> pos = pos.copy(x = pos.x + 1)
                2 -> pos = pos.copy(y = pos.y - 1)
                3 -> pos = pos.copy(x = pos.x - 1)
            }

            if (places.contains(pos)) {
                println(Math.abs(pos.x) + Math.abs(pos.y))
                exitProcess(0)
            }

            places.add(pos)

        }

    }


}