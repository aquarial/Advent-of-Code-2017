fun main(args: Array<String>) {
    val readFromStdIn = mutableListOf<String>()
    while (true) {
        val line = readLine()!!
        if (line == "done") break
        readFromStdIn.add(line)
    }

    fun f(s: String, i: Int = 0) = s.split("-")[i].toLong()

    readFromStdIn.sortBy { f(it) }

    println(readFromStdIn.take(10))


    var i = 0
    var maxForward = f(readFromStdIn[0], 1)
    var count = 0L

    try {

        while (true) {

            while (maxForward >= f(readFromStdIn[i + 1])) {
                i++
                maxForward = Math.max(maxForward, f(readFromStdIn[i], 1))
            }

            count += f(readFromStdIn[i + 1]) - maxForward - 1

            i++
            maxForward = Math.max(maxForward, f(readFromStdIn[i], 1))
        }
    } catch (e: Exception) {
        println(count)
        println(maxForward)
        println(4294967295L)
    }
}

// < 230