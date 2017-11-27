package y2016


import kotlin.system.exitProcess

@Suppress("LiftReturnOrAssignment")
fun main(args: Array<String>) {

    val instructions = mutableListOf<String>()
    while (true) {
        val line = readLine() ?: break
        instructions.add(line)
    }

    data class State(val reges: List<Int>, val p_backup: Int)

    aLoop@ for (a_start in 0..5000) {
        if (a_start % 100 == 0)
            println(a_start)
        var pointer = 0
        val registers = mutableListOf(a_start, 0, 0, 0)
        val outPut = mutableListOf<Int>()

        var lastSize = -1
        while (pointer < instructions.size) {


            if (outPut.size != lastSize && (outPut.size + 1) % 10 == 0) {
                if (outPut.filterIndexed({ index: Int, v: Int -> index % 2 != v }).isEmpty())
                    println("$a_start gives $outPut")
                else
                    continue@aLoop
            }


            val instr = instructions[pointer].split(" ")
            val firstChar = instr[1].toCharArray()[0]

            try {

                if (instr[0] == "out") {
                    val outer = registers["abcd".indexOf(firstChar)]
                    outPut.add(outer)
                    pointer++
                }


                if (instr[0] == "tgl") {
                    println("UNKNOWN")
                    exitProcess(1)
                }

                if (instr[0] == "cpy") {
                    val secondChar = instr[2].toCharArray()[0]
                    var loaded = 0

                    if (firstChar in "abcd") {
                        loaded = registers["abcd".indexOf(firstChar)]
                    } else {
                        loaded = instr[1].toInt()
                    }

                    registers["abcd".indexOf(secondChar)] = loaded
                    pointer++
                }

                if (instr[0] == "jnz") {
                    if (firstChar == '0')
                        pointer++
                    else {
                        val reg = "abcd".indexOf(firstChar)
                        if (reg == -1 || registers[reg] != 0) {
                            if (instr[2].toCharArray()[0] in "abcd")
                                pointer += registers["abcd".indexOf(instr[2].toCharArray()[0])]
                            else
                                pointer += instr[2].toInt()
                        } else {
                            pointer++
                        }
                    }
                }

                if (instr[0] == "inc") {
                    registers["abcd".indexOf(firstChar)]++
                    pointer++
                }
                if (instr[0] == "dec") {
                    registers["abcd".indexOf(firstChar)]--
                    pointer++
                }

                if (instr[0] == "NOTHING")
                    pointer++

            } catch (e: Exception) {
                e.printStackTrace()
                pointer++
            }

        }
    }
}