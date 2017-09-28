
fun main(args: Array<String>) {

    var count = 0
    while (true) {

        val line = readLine()!!

        val outsidebrackets = line.split("\\[.*?]".toRegex()) // "]" or "["
        val insidebrackets = ("]$line[").split("].*?\\[".toRegex())


        val abas = mutableListOf<String>()
        for (part in outsidebrackets) {
            val local_abas = (0 until part.length)
                    .map { tlsAt(part, it) }
                    .filterNotNull()
                    .toList()
            abas.addAll(local_abas)
        }

        println("abas = " + abas)

        var supported = false
        for (part in insidebrackets) {
            abas
                    .filter { part.contains(Regex.fromLiteral(it)) }
                    .forEach { supported = true }
        }

        if (supported)
            count++

        println("count = " + count)
        println()
    }
}

fun tlsAt(str: String, location: Int): String? {
    val str2 = str.drop(location).take(3)
    if (str2.length == 3 && str2[0] == str2[2])
        return "" + str2[1] + str2[0] + str2[1]

    return null
}

/*


aba[bab]xyz
xyx[xyx]xyx
aaa[kek]eke
zazbz[bzb]cdb




 */