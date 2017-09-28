import java.math.BigInteger
import java.nio.charset.StandardCharsets
import java.security.MessageDigest

fun main(args: Array<String>) {
    val password = mutableListOf('_', '_', '_', '_', '_', '_', '_', '_')
    println(password)
    var b = BigInteger("0")
    while (true) {
        val str = md5("wtnhxymk" + b.toString())
        if (isInteresting(str)) {
            val index = str[5].toInt() - '0'.toInt()
            val char = str[6]


            print(password)
            if (index in 0..7) {
                if (password[index] == '_')
                    password[index] = char
                print("updated [$index] = $char")
            } else {
                print("invalid index: $index")
            }
            println()
        }
        b += BigInteger.ONE
    }
}

fun isInteresting(str: String): Boolean = str.take(5) == "00000"


val md5 = MessageDigest.getInstance("MD5")!!
fun md5(str: String): String {
    md5.reset()
    md5.update(StandardCharsets.UTF_8.encode(str))
    return String.format("%032x", BigInteger(1, md5.digest()))
}