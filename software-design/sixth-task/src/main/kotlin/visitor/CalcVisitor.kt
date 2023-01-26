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

class CalcVisitor : TokenVisitor {
    private val deque: Deque<Int> = ArrayDeque()

    fun getResult(): Int = deque.last

    override fun visit(token: NumberToken) {
        deque.addLast(token.value)
    }

    override fun visit(token: BraceToken) {
        throw IllegalArgumentException("Unexpected token $token")
    }

    override fun visit(token: OperationToken) {
        if (deque.size < 2) {
            throw IllegalArgumentException("Wrong number of arguments for operation $token")
        }

        val x = deque.removeLast()
        val y = deque.removeLast()
        val op = getOperation(token.operation)

        deque.addLast(op(y, x))
    }

    companion object {
        fun getOperation(operation: Operation): (Int, Int) -> Int = when (operation) {
            is Plus -> Int::plus
            is Mul -> Int::times
            is Div -> Int::div
            is Minus -> Int::minus
        }
    }
}