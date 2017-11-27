package y2016


fun main(args: Array<String>) {

    fun charFromS(str: String): String {
        if (str.length < 3)
            return ""

        // TRAPS ARE TRUE
        val l = str[0] == '^'
        val c = str[1] == '^'
        val r = str[2] == '^'

        if (l && c && !r)
            return "^"

        if (!l && c && r)
            return "^"

        if (l && !c && !r)
            return "^"

        if (!l && !c && r)
            return "^"

        return "."
    }

    var row = ".^^.^^^..^.^..^.^^.^^^^.^^.^^...^..^...^^^..^^...^..^^^^^^..^.^^^..^.^^^^.^^^.^...^^^.^^.^^^.^.^^.^."
    val iters = 400000 - 1// 40 - 1
    println(row)
    println()

    var count = row.count { it == '.' }

    for (tempVar in 1..iters) {
        row = ".$row."
        val nextRow = StringBuilder()
        for (i in 0 until row.length) {
            nextRow.append(charFromS(row.substring(i)))
        }

        row = nextRow.toString()

        count += row.count { it == '.' }
    }

    println(count)
}