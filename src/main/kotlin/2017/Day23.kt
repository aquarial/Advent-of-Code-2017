@Suppress("LiftReturnOrAssignment")
fun main(args: Array<String>) {

    val instructions = mutableListOf<String>()
    while (true) {
        val line = readLine() ?: break
        instructions.add(line)
    }

    var pointer = 0
    val registers = mutableListOf(3, 0, 0, 0)

    while (pointer < instructions.size) {

        val instr = instructions[pointer].split(" ")
        val firstChar = instr[1].toCharArray()[0]


        try {
            if (instr[0] == "tgl") {
                val secondChar = instr[1].toCharArray()[0]
                val dist = registers["abcd".indexOf(secondChar)]

                val nextI = instructions[pointer + dist].split(" ").toMutableList()
                println("pointer=$pointer changing $nextI")
                if (nextI.size == 2) {
                    if (nextI[0] == "inc")
                        nextI[0] = "dec"
                    else
                        nextI[0] = "inc"
                } else {
                    if (nextI[0] == "jnz")
                        nextI[0] = "cpy"
                    else
                        nextI[0] = "jnz"
                }

                instructions[pointer + dist] = nextI.joinToString(separator = " ")
                pointer++
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

            if (instr[0] == "inc") {
                registers["abcd".indexOf(firstChar)]++
                pointer++
            }
            if (instr[0] == "dec") {
                registers["abcd".indexOf(firstChar)]--
                pointer++
            }

        } catch (e: Exception) {
            e.printStackTrace()
            pointer++
        }

    }

    println(instructions)
    println()
    println(registers)
}