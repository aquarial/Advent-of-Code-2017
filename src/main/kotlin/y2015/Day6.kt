package y2015

fun main(args: Array<String>) {

    val lights = MutableList(1000, {
        MutableList(1000, {
            0
        })
    })



    while (true) {
        val input = readLine()!!
        if (input == "break") break

        println(input)

        Regex("(turn on|toggle|turn off) (\\d+),(\\d+) through (\\d+),(\\d+)")
                .matchEntire(input)?.let {
            val v = it.groupValues.drop(2).map { it.toInt() }
            for (x in v[0]..v[2]) {
                for (y in v[1]..v[3]) {

                    if (it.groupValues[1] == "turn on")
                        lights[y][x]++
                    if (it.groupValues[1] == "turn off")
                        lights[y][x] = Math.max(0, lights[y][x] - 1)
                    if (it.groupValues[1] == "toggle")
                        lights[y][x] += 2


                }
            }


        }

    }

    println(lights.map { it.sum() }.sum())

}