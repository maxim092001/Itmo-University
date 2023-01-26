package visitor

import token.Token.Companion.BraceToken
import token.Token.Companion.NumberToken
import token.Token.Companion.OperationToken

interface TokenVisitor {

    fun visit(token: NumberToken)

    fun visit(token: BraceToken)

    fun visit(token: OperationToken)

}