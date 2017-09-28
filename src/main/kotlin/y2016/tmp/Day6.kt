fun main(args: Array<String>) {

    // ewqplnag
    val word = mutableListOf<MutableList<Char>>()
    for (i in 1..8)
        word.add(mutableListOf())

    while (true) {
        readLine()!!.mapIndexed { i, c -> word[i].add(c) }
        println("words === $word")
        println(word.map { repeatChar(it) })
    }
}


fun repeatChar(chars: List<Char>): List<Char>? {
    val grouped0 = chars
            .groupBy { it }.values
    println("asdf $grouped0")
    val grouped = grouped0
            .toMutableList()

            .groupBy({ charList -> charList.size }, { charList -> charList[0] })

    println("asdf $grouped")
    println()
    val lengths = grouped.keys.toList().sorted()

    return grouped[lengths[0]]
}