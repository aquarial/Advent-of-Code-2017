package y2015

import y2016.md5

fun main(args: Array<String>) {
    val input = "ckczppom"

    for (i in 1..Int.MAX_VALUE) {
        if (md5(input + i).startsWith("000000")) {
            println(i)
            break
        }
    }
}