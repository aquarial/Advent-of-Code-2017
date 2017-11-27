package y2016


import jdk.nashorn.internal.objects.NativeArray.shift

fun main(args: Array<String>) {
    var count = 0


    while (true) {
        val line = readLine()!!

        val name = line.takeWhile { it != '[' }.filter { Character.isLetter(it) }
        val listedCheckSum = ".*?\\[(.*?)]".toRegex().matchEntire(line)!!
                .groupValues[1]
        val sectorId = ".*?-([^-]*)\\[.*?".toRegex().matchEntire(line)!!
                .groupValues[1].toInt()

        val nameChars = name
                .toList()
                .groupBy { it }.values
                .toMutableList()


                .groupBy({ chars -> chars.size }, { chars -> chars[0] })


        val tmpcheckSum = mutableListOf<Char>()
        val lengths = nameChars.keys.toList().sorted().reversed()
        for (key in lengths) {
            tmpcheckSum.addAll(nameChars[key]!!.sorted())
        }
        val checkSum = tmpcheckSum.take(5)

        if (checkSum == listedCheckSum.toList()) {
            println("${name.map { shift(it, sectorId % 26) }}  --- $sectorId")
        }
    }
}


fun shift(c: Char, shift: Int): Char {
    if (c == '-')
        return ' '
    else
        return ((c.toInt() - 'a'.toInt() + shift) % 26 + 'a'.toInt()).toChar()
}