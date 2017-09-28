package y2015

fun main(args: Array<String>) {
    val input = "abcdef"

    for (i in 1..Int.MAX_VALUE) {
        if (md5(input + i))
    }
}