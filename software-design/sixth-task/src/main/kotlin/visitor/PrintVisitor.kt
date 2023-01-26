package visitor

import token.Token.Companion.BraceToken
import token.Token.Companion.NumberToken
import token.Token.Companion.OperationToken

class PrintVisitor : TokenVisitor {
    override fun visit(token: NumberToken) {
        print(" $token ")
    }

    override fun visit(token: BraceToken) {
        print(" $token ")
    }

    override fun visit(token: OperationToken) {
        print(" $token ")
    }
}