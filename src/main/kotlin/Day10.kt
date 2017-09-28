import java.io.BufferedReader
import java.io.InputStreamReader
import java.net.URL
import java.nio.file.Files
import kotlin.system.exitProcess

fun main(args: Array<String>) {

    val readInstructions = mutableListOf<String>()
    while (true) {
        val line = readLine()!!
        if (line == "done") break
        readInstructions.add(line)
    }

    val instructions = readInstructions.map { parseCommand(it) }.toMutableList()
    val not_interpretated = mutableListOf<Command>()

    val bots = MutableList(500, { _ -> mutableListOf<Int>() })
    val outputs = MutableList(500, { _ -> mutableListOf<Int>() })

    while (instructions.size > 0) {
        println(bots)
        for (instr in instructions) {
            var unused = true
            when (instr) {
                is ValGoes -> {
                    bots[instr.bot].add(instr.num)
                    unused = false
                }
                is BotGives -> {
                    if (bots[instr.bot].size == 2) {
                        bots[instr.bot].sort()

                        val low = bots[instr.bot].removeAt(0)
                        val high = bots[instr.bot].removeAt(0)

                        if (instr.lowTarget >= 0)
                            bots[instr.lowTarget].add(low)
                        else
                            outputs[-(instr.lowTarget+1)].add(low)

                        if (instr.highTarget >= 0)
                            bots[instr.highTarget].add(high)
                        else
                            outputs[-(instr.highTarget+1)].add(high)

                        unused = false

                    }
                }
            }

            if (unused)
                not_interpretated.add(instr)

        }
        instructions.clear()
        instructions.addAll(not_interpretated)
        not_interpretated.clear()
    }
    println(outputs)

}


sealed class Command
data class ValGoes(val num: Int, val bot: Int) : Command()
data class BotGives(val bot: Int, val lowTarget: Int, val highTarget: Int) : Command()


val goes_regex = Regex("value (\\d+) goes to bot (\\d+)")
val bot_give_regex = Regex("bot (\\d+) gives low to (\\w+) (\\d+) and high to (\\w+) (\\d+)")

fun parseCommand(str: String): Command {
    goes_regex.matchEntire(str)?.groupValues?.let {
        return ValGoes(it[1].toInt(), it[2].toInt())
    }

    bot_give_regex.matchEntire(str)?.groupValues?.let {
        val target1 = if (it[2] == "bot") it[3].toInt() else (-it[3].toInt() - 1)
        val target2 = if (it[4] == "bot") it[5].toInt() else (-it[5].toInt() - 1)
        return BotGives(it[1].toInt(), target1, target2)
    }
    println("Unable to parse $str")
    exitProcess(1)
}
