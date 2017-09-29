package y2015

fun main(args: Array<String>) {

    fun niceString(str:String): Boolean {
        if (str.count {"aeiou".contains(it)} < 3)
            return false

        listOf("ab", "cd", "pq", "xy")
                .filter { str.contains(it) }
                .forEach { return false }


        return "abcdefghijklmnopqrstuvwxyz"
                .map {"$it$it"}
                .any {str.contains(it)}
    }

    var count = 0
    while (true) {
        val input = readLine()!!

        if (niceString(input))
            count++

        println(count)
    }
}