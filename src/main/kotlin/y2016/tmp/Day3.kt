fun main(args: Array<String>) {
    var count = 0
    while (true) {

        val stack = mutableListOf<List<Int>>()

        for (i in 1..3) {
            val line = readLine()!!
            val input = line
                    .replace("^ +".toRegex(), "")
                    .split(" +".toRegex())
                    .map { it.toInt() }
            stack.add(input)
        }

        for (i in 0..2) {
            val tris = listOf(stack[0][i], stack[1][i], stack[2][i])
                    .sorted()

            if (tris[0] + tris[1] > tris[2]) {
                count++
                println("$tris - $count")
            }
        }
        stack.clear()
    }

} // 880 882