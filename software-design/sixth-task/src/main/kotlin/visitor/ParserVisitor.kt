package visitor

import token.Token
import java.util.*
import token.Token.Companion.NumberToken
import token.Token.Companion.BraceToken
import token.Token.Companion.OperationToken
import token.Brace.Companion.LeftBrace
import token.Brace.Companion.RightBrace
import token.Operation

import token.Operation.Companion.Plus
import token.Operation.Companion.Minus
import token.Operation.Companion.Mul
import token.Operation.Companion.Div

class ParserVisitor : TokenVisitor {
    private val deque: Deque<Token> = ArrayDeque()
    private val result: MutableList<Token> = mutableListOf()

    override fun visit(token: NumberToken) {
        result.add(token)
    }

    fun getResult(): List<Token> {
        while (deque.isNotEmpty()) {
            when (val t = deque.removeLast()) {
                is OperationToken -> result.add(t)
                else -> throw IllegalStateException("Unexpected token $t")
            }
        }
        return result
    }

    override fun visit(token: BraceToken) {
        when (token.brace) {
            is LeftBrace -> deque.addLast(token)
            is RightBrace -> {
                while (deque.isNotEmpty()) {
                    when (val t = deque.removeLast()) {
                        is OperationToken -> result.add(t)
                        is BraceToken -> {
                            when (t.brace) {
                                is LeftBrace -> break
                                is RightBrace -> throw IllegalStateException("Unexpected token: $token")
                            }
                        }
                        is NumberToken -> throw IllegalStateException("Unexpected token: $token")
                    }
                }
            }
        }
    }

    override fun visit(token: OperationToken) {
        while (deque.isNotEmpty()) {
            when (val t = deque.last) {
                is OperationToken -> {
                    if (getPriority(t.operation) >= getPriority(token.operation)) {
                        result.add(t)
                        deque.removeLast()
                    } else {
                        break
                    }
                }
                else -> break
            }
        }
        deque.addLast(token)
    }

    companion object {
        fun getPriority(operation: Operation): Int = when (operation) {
            is Plus, is Minus -> 1
            is Mul, is Div -> 2
        }
    }
}