package y2015

fun main(args: Array<String>) {

    fun niceString(str: String): Boolean {
        val oneLetter = (0..str.lastIndex - 2)
                .any { str[it] == str[it + 2] }


        val pairs = mutableListOf<String>()
        val iterator = str.toList().listIterator()
        while (iterator.hasNext()) {
            val start = iterator.next()

            if (iterator.hasNext()) {
                val second = iterator.next()
                pairs.add("" + start + second)

                if (iterator.hasNext() && start == second && second == str[iterator.nextIndex()])
                    iterator.next()

                iterator.previous()
            }
        }

        val pair = pairs.size != pairs.toSet().size

        println(pairs)
        println("$pair   $oneLetter")
        return pair && oneLetter
    }

    var count = 0
    while (true) {
        val input = readLine()!!

        if (niceString(input))
            count++

        println(count)
    }
}