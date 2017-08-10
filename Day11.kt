@file:Suppress("LoopToCallChain")

import kotlin.system.exitProcess

fun main(args: Array<String>) {
    val states = mutableListOf(State(1, listOf(1, 1, 1, 1, 1, 3, 3), listOf(1, 1, 1, 2, 2, 3, 3)))
    val nextStates = mutableSetOf<State>()

    val seen = HashSet<State>()

    var count = 0
    while (true) {
        seen.addAll(states)
        for (s in states) {
            if (s.isWin()) {
                println(s)
                exitProcess(0)
            }
            s.nextPossibleStates()
                    .filter { it.isValid() }
                    .filter { it !in seen }
                    .forEach { nextStates.add(it) }
        }

        count++
        println("After $count, there are ${nextStates.size} possible states")
        Thread.sleep(500)

        states.clear()
        states.addAll(nextStates)
        nextStates.clear()
    }
}

sealed class Index
data class GenIndex(val index: Int) : Index()
data class ChipIndex(val index: Int) : Index()

data class State(val elevatorPos: Int, val generators: List<Int>, val chips: List<Int>) {


    fun isWin(): Boolean = generators.all { it == 4 } && chips.all { it == 4 }

    @Suppress("ReplaceRangeToWithUntil")
    fun nextPossibleStates(): List<State> {
        val indices = mutableListOf<Index>()
        val indexPairs = mutableListOf<Pair<Index, Index>>()

        for (i in 0 until generators.size) {
            if (generators[i] == elevatorPos)
                indices.add(GenIndex(i))
        }
        for (i in 0 until chips.size) {
            if (chips[i] == elevatorPos)
                indices.add(ChipIndex(i))
        }

        for (i in 0..indices.size - 1) {
            for (j in i..indices.size - 1) {
                indexPairs.add(Pair(indices[i], indices[j]))
            }
        }

        fun moveIndex(loc: Index, newPos: Int, gens: MutableList<Int>, chips: MutableList<Int>) {
            if (loc is GenIndex)
                gens[loc.index] = newPos
            if (loc is ChipIndex)
                chips[loc.index] = newPos
        }

        val nextStates = mutableListOf<State>()
        for (i in indexPairs) {
            val localGen = generators.toMutableList()
            val localChips = chips.toMutableList()

            moveIndex(i.first, elevatorPos + 1, localGen, localChips)
            moveIndex(i.second, elevatorPos + 1, localGen, localChips)
            nextStates.add(State(elevatorPos + 1, localGen.toList(), localChips.toList()))

            moveIndex(i.first, elevatorPos - 1, localGen, localChips)
            moveIndex(i.second, elevatorPos - 1, localGen, localChips)
            nextStates.add(State(elevatorPos - 1, localGen.toList(), localChips.toList()))
        }

        return nextStates
    }

    /**
     * If this state is a valid state. Looks at elevator pos and whether chips are fried
     */
    fun isValid(): Boolean {
        if (elevatorPos !in 1..4)
            return false


        val generatorsOnFloor = List(5, { i -> generators.any { it == i } })

        for (floor in 1..4) {
            for (type in 0 until chips.size) {
                if (chips[type] == floor && generators[type] != floor && generatorsOnFloor[floor]) {
                    return false
                }
            }
        }

        return true
    }
}