package y2016


fun main(args: Array<String>) {


    fun hasher(str: String, iters: Int): String {
        var s = str
        for (i in 1..iters) {
            s = md5(s)
        }
        return s
    }

    val input = "cuanljph"

    val maybeKeys = mutableListOf<MaybeKey>()
    val keys = mutableListOf<Key>()

    var index = 0
    while (true) {
        val str = hasher(input + index, 2017)
        val res = repeaters(str)

        maybeKeys.removeAll { it.endIndex < index }

        val toRemove = mutableListOf<MaybeKey>()
        maybeKeys.forEach { maybeK ->
            if (res.second.any { it.character == maybeK.character }) {
                toRemove.add(maybeK)
                keys.add(Key(index, maybeK))
            }
        }
        toRemove.forEach { maybeKeys.remove(it) }

        if (res.first.isNotEmpty()) {
            maybeKeys.add(MaybeKey(index, index + 1000, res.first[0].character))
        }

        keys.sortBy { it.index }
//        println("index:" + index + "  -  key:" + keys.size + "  -  keys" + keys)
        if (keys.size >= 64) {
            println("Answer = ${keys[63]}")
            break
        }

        if (index % 1000 == 0) {
            println(index)
        }

        index++
    }
}

fun repeaters(str: String): Pair<List<Repeater>, List<Repeater>> {

    val threers = mutableListOf<Repeater>()
    val fivers = mutableSetOf<Repeater>()

    char@ for (i in 0 until str.length) {
        var j = 1

        while (i + j < str.length && str[i] == str[i + j]) {
            j++
        }

        if (j >= 3)
            threers.add(Repeater(str[i], 3))

        if (j >= 5)
            fivers.add(Repeater(str[i], 5))
    }

    return Pair(threers, fivers.toList())
}
// not 24769
//

data class Repeater(val character: Char, val repeats: Int)

data class MaybeKey(val index: Int, val endIndex: Int, val character: Char)
data class Key(val index: Int, val src: MaybeKey)