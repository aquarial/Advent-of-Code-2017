fun main(args: Array<String>) {
    var start = 3014603 - 3.0
//    var start = 36 - 3.0

    var i = 1.0
    while (start > 0) {
        start -= 2.0 * Math.pow(3.0, i)
        i++

    }

//    println(i - 1)
    println(start + 2.0 * Math.pow(3.0, i - 1))
}