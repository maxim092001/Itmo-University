package token

import java.io.InputStream
import token.Token.Companion.OperationToken

import token.Token.Companion.BraceToken
import token.Token.Companion.NumberToken
import token.Operation.Companion.Plus
import token.Operation.Companion.Div
import token.Operation.Companion.Mul
import token.Operation.Companion.Minus
import token.Brace.Companion.LeftBrace
import token.Brace.Companion.RightBrace

class Tokenizer(private val inputStream: InputStream) {

    private val tokens: MutableList<Token> = arrayListOf()
    private var currentPos: Int = 0
    private var currentChar: Int = 0

    init {
        getChar()
    }

    private fun getChar() {
        currentChar = inputStream.read()
        currentPos++
    }

    private fun skipWhitespaces() {
        while (currentChar.toChar().isWhitespace()) {
            getChar()
        }
    }

    private fun getNumber(): Int {
        val stringBuilder: StringBuilder = StringBuilder()

        while (currentChar.toChar().isDigit()) {
            stringBuilder.append(currentChar.toChar())
            getChar()
        }
        return stringBuilder.toString().toInt()
    }

    fun getTokens(): List<Token> {
        while (true) {
            skipWhitespaces()

            if (currentChar == -1) {
                break
            }

            val token = when (currentChar.toChar()) {
                '+' -> OperationToken(Plus)
                '-' -> OperationToken(Minus)
                '*' -> OperationToken(Mul)
                '/' -> OperationToken(Div)
                '(' -> BraceToken(LeftBrace)
                ')' -> BraceToken(RightBrace)
                else -> {
                    if (currentChar.toChar().isDigit()) {
                        val t = NumberToken(getNumber())
                        tokens.add(t)
                        continue
                    } else {
                        throw IllegalStateException("Illegal character ${currentChar.toChar()} at pos $currentPos")
                    }
                }
            }
            tokens.add(token)
            getChar()
        }
        return tokens
    }
}