fun main(args: Array<String>) {
    fun isGood(x: Int): Boolean {
        fun expect(shift: Int, mod: Int) = (x + shift) % mod != 0

        if (expect(15 + 1, 17)) return false
        if (expect(2 + 2, 3)) return false
        if (expect(4 + 3, 19)) return false
        if (expect(2 + 4, 13)) return false
        if (expect(2 + 5, 7)) return false
        if (expect(0 + 6, 5)) return false


        // part 2
        if (expect(0 + 7, 11)) return false

            return true
    }


    var index = 0

    while (true) {
        if (isGood(index)) {
            println(index)
            break
        }
        if (index % 1000 == 0)
            println("ANOTHER $index")
        index++
    }
//            .filter { isGood(it) }.forEach { println(it) }

}
// 29309
