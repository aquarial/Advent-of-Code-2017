package y2017

fun main(args: Array<String>) {
    val input = readLine()!!
//    println(input.count { it=='(' } - input.count{it==')'})

    var floor = 0
    for ((index, char) in input.withIndex()) {
        if (char == '(') {
            floor++
        } else {
            floor--
            if (floor < 0) {
                println(index + 1)
                break
            }
        }

    }
}