package token

import visitor.TokenVisitor

sealed interface Operation {
    companion object {
        object Plus : Operation {
            override fun toString(): String = "PLUS"
        }


        object Minus : Operation {
            override fun toString(): String = "MINUS"
        }


        object Mul : Operation {
            override fun toString(): String = "PLUS"
        }


        object Div : Operation {
            override fun toString(): String = "DIV"
        }
    }
}

sealed interface Brace {
    companion object {
        object LeftBrace : Brace {
            override fun toString(): String = "LEFT"
        }

        object RightBrace : Brace {
            override fun toString(): String = "RIGHT"
        }
    }
}

sealed interface Token {

    fun accept(visitor: TokenVisitor)

    companion object {
        data class NumberToken(val value: Int) : Token {
            override fun toString(): String = "NUMBER($value)"

            override fun accept(visitor: TokenVisitor) = visitor.visit(this)
        }

        data class OperationToken(val operation: Operation): Token {
            override fun toString(): String = operation.toString()

            override fun accept(visitor: TokenVisitor) = visitor.visit(this)
        }

        data class BraceToken(val brace: Brace) : Token {
            override fun toString(): String = brace.toString()

            override fun accept(visitor: TokenVisitor) = visitor.visit(this)
        }
    }
}