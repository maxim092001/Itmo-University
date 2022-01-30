import java.text.ParseException;

// todo fix assoc + diff prior or/xor
/*
    E -> T X
    X -> xor T X
    X -> EPS
    T -> M Z
    Z -> or M Z
    Z -> EPS
    M -> G Y
    Y -> and G Y
    Y -> EPS
    G -> not G
    G -> (E)
    G -> VAR K
    K -> not I
    K -> in VAR
    K -> EPS
    I -> not I
    I -> in VAR
 */
public class Parser {

    private Lexer lexer;
    private static final String E = "E";
    private static final String T = "T";
    private static final String X = "X";
    private static final String Z = "Z";
    private static final String M = "M";
    private static final String Y = "Y";
    private static final String G = "G";
    private static final String I = "I";
    private static final String K = "K";
    private static final String EPS = "EPS";

    public Tree parse(final String s) throws ParseException {
        this.lexer = new Lexer(s);
        lexer.nextToken();
        Tree e = E();
        if (!lexer.isEmpty()) {
            throw createParserException("Undefined");
        }
        return e;
    }

    private Tree E() throws ParseException {
        var t = T();
        var x = X();
        return Tree.of(E, t, x);
    }

    private Tree X() throws ParseException {
        if (lexer.getCurrentToken() == Token.XOR) {
            lexer.nextToken();
            var t = T();
            return Tree.of(X, t, Tree.of(Token.XOR.name()), X());
        }
        return Tree.of(X, Tree.of(EPS));
    }

    private Tree T() throws ParseException {
        var m = M();
        var z = Z();
        return Tree.of(T, m, z);
    }

    private Tree Z() throws ParseException {
        if (lexer.getCurrentToken() == Token.OR) {
            lexer.nextToken();
            var m = M();
            return Tree.of(Z, m, Tree.of(Token.OR.name()), Z());
        }
        return Tree.of(Z, Tree.of(EPS));
    }

    private Tree M() throws ParseException {
        var g = G();
        return Tree.of(M, g, Y());
    }

    private Tree Y() throws ParseException {
        if (lexer.getCurrentToken() == Token.AND) {
            lexer.nextToken();
            var g = G();
            return Tree.of(Y, Tree.of(Token.AND.name()), g, Y());
        }
        return Tree.of(Y, Tree.of(EPS));
    }

    private Tree G() throws ParseException {
        if (lexer.getCurrentToken() == Token.VAR) {
            lexer.nextToken();
            return Tree.of(G, Tree.of(Token.VAR.name()), K());
        }
        if (lexer.getCurrentToken() == Token.LPAREN) {
            lexer.nextToken();
            var e = E();
            if (lexer.getCurrentToken() == Token.RPAREN) {
                lexer.nextToken();
                return Tree.of(G, Tree.of(Token.LPAREN.name()), e, Tree.of(Token.RPAREN.name()));
            }
            throw createParserException("Wrong parentheses");
        }
        if (lexer.getCurrentToken() == Token.NOT) {
            lexer.nextToken();
            return Tree.of(G, Tree.of(Token.NOT.name()), G());
        }
        throw createParserException("Undefined error");
    }

    private Tree K() throws ParseException {
        if (lexer.getCurrentToken() == Token.NOT) {
            lexer.nextToken();
            return Tree.of(K, Tree.of(Token.NOT.name(), I()));
        }
        if (lexer.getCurrentToken() == Token.IN) {
            lexer.nextToken();
            if (lexer.getCurrentToken() == Token.VAR) {
                return Tree.of(K, Tree.of(Token.IN.name()), Tree.of(Token.VAR.name()));
            } else {
                throw createParserException("No argument for in");
            }
        }
        return Tree.of(K, Tree.of(EPS));
    }

    private Tree I() throws ParseException {
        if (lexer.getCurrentToken() == Token.IN) {
            lexer.nextToken();
            if (lexer.getCurrentToken() == Token.VAR) {
                lexer.nextToken();
                return Tree.of(I, Tree.of(Token.IN.name()), Tree.of(Token.VAR.name()));
            } else {
                throw createParserException("No argument for in");
            }
        }
        if (lexer.getCurrentToken() == Token.NOT) {
            lexer.nextToken();
            return Tree.of(I, Tree.of(Token.NOT.name()), I());
        }
        throw createParserException("Error in I");
    }

    private ParseException createParserException(final String message) {
        return new ParseException(message, lexer.getCurrentIndex());
    }
}
