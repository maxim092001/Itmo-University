import token.Tokenizer
import visitor.CalcVisitor
import visitor.ParserVisitor
import visitor.PrintVisitor
import java.nio.charset.Charset

fun main() {
    val input = readLine() ?: throw IllegalArgumentException("Non-null input expected")
    val inputStream = input.byteInputStream(Charset.defaultCharset())
    val tokens = Tokenizer(inputStream).getTokens()

    println("Tokens: ")
    println(tokens.joinToString(" "))

    val parserVisitor = ParserVisitor()
    tokens.forEach {
        it.accept(parserVisitor)
    }
    val rpnTokens = parserVisitor.getResult()

    println("Tokens in reverse polish notation:")
    val printVisitor = PrintVisitor()
    rpnTokens.forEach {
        it.accept(printVisitor)
    }
    println()

    val calcVisitor = CalcVisitor()
    rpnTokens.forEach {
        it.accept(calcVisitor)
    }

    println("Result = ${calcVisitor.getResult()}")
}