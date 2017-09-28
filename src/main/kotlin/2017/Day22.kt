import kotlin.system.exitProcess

fun main(args: Array<String>) {

    data class Pos(val x: Int, val y: Int)
    data class Node(val p: Pos, val size: Int, val used: Int, val available: Int)

    fun parseNode(str: String): Node {
        Regex("/dev/grid/node-x(\\d+)-y(\\d+)\\s+(\\d+)T\\s+(\\d+)T\\s+(\\d+)T.*").matchEntire(str)?.groupValues?.let {
            return Node(Pos(it[1].toInt(), it[2].toInt()), it[3].toInt(), it[4].toInt(), it[5].toInt())
        }
        println("Couln't parse $str")
        exitProcess(1)
    }

    val readInstructions = mutableListOf<Node>()
    while (true) {
        val line = readLine() ?: break
        readInstructions.add(parseNode(line))
    }


    for (yy in 0..100) {
        readInstructions.filter { it.p.y == yy }.sortedBy { it.p.x }.forEach { print("" + it.used + "/" + it.size + "  ") }
        println()
    }

    // after printing this info out, I looked through and only saw the one spike from bottom right to middle of
    // 250T used nodes, so I assumed all the other movements would work out and charted a possible course of 254
    // movements. This was too high. Because I used an increment of 5 movements to move the top right one node to
    // the left, the next guess I gave was 249, which was the correct answer


//    val used = readInstructions.sortedBy { it.used }.filter { it.used != 0 }
//    val avail = readInstructions.sortedBy { it.available }
//
//    var pairiings = 0
//
//    var avail_index = 0
//    for (node in used) {
//        while (avail[avail_index].available < node.used || avail[avail_index] == node) {
//            avail_index++
//            if (avail_index >= avail.size) {
//                println(pairiings)
//                exitProcess(0)
//            }
//        }
//
//        pairiings += avail.size - avail_index
//    }
//
//    println(pairiings)
}
