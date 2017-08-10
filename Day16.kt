fun main(args: Array<String>) {
    var state = "11110010111001001"
    val diskSpace = 35651584
    // val diskSpace = 272

    while (state.length <= diskSpace) {
        println("Len=${state.length}")
        val a = state
        val b = state.reversed()
                .replace('0', 't')
                .replace('1', '0')
                .replace('t', '1')

        state = a + '0' + b
    }

    println("state=$state")
    state = state.take(diskSpace)
    println("state=$state")

    while (state.length % 2 == 0) {
        val nextState = StringBuilder()

        for (i in (0..state.length - 2).filter { it % 2 == 0 }) {
            if (state[i] == state[i + 1])
                nextState.append('1')
            else
                nextState.append('0')
        }

        state = nextState.toString()
        println("shorten=$state")
    }

    println(state)
}