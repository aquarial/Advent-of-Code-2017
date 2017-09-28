@Suppress("LiftReturnOrAssignment")
fun main(args: Array<String>) {

    val readInstructions = mutableListOf<String>()
    while (true) {
        val line = readLine()!!
        if (line == "done") break
        readInstructions.add(line)
    }

    val instructions = readInstructions.toList()

    var pointer = 0
    val registers = mutableListOf(0, 0, 1, 0)

    while (pointer < instructions.size) {

        val instr = instructions[pointer].split(" ")
        val firstChar = instr[1].toCharArray()[0]


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

    }

    println(registers)
}