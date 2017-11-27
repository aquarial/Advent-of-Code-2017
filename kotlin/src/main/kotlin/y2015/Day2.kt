package y2015

fun main(args: Array<String>) {

    var total = 0
    while (true) {
        val input = readLine()!!.split("x").map { it.toInt() }.sorted()

//        total += input[0] * input[1]
//        total += 2*input[0]*input[1]
//        total += 2*input[0]*input[2]
//        total += 2*input[1]*input[2]

        total += 2 * input[0] + 2 * input[1] + input[0] * input[1] * input[2]

        println(total)
    }
}