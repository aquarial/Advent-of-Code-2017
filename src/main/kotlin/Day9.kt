import kotlin.system.exitProcess


val regex = Regex("\\((\\d+)x(\\d+)\\)")

fun main(args: Array<String>) {
    val str = readLine()!!

    println(lengthStr(str))
//    var len = 0
//    var prevMatch = 0
//
//    while (prevMatch < str.length) {
//
//        val result = regex.find(str, prevMatch)
//        if (result == null) {
//            println(len + str.length - prevMatch)
//            exitProcess(0)
//        }
//        len += result.range.start - prevMatch
//        len += result.groupValues[1].toInt() * result.groupValues[2].toInt()
//
//        prevMatch = result.range.endInclusive + 1 + result.groupValues[1].toInt()
//    }
//    println(len + str.length - prevMatch)
}

fun lengthStr(str: String): Long {
    val result = regex.find(str)
    if (result == null) {
        return str.length.toLong()
    } else {
        val endExclusive = result.range.endInclusive + 1
        return result.range.start +
                result.groupValues[2].toInt() *
                        lengthStr(str.substring(endExclusive, endExclusive + result.groupValues[1].toInt())) +
                lengthStr(str.substring(endExclusive + result.groupValues[1].toInt()))
    }

}