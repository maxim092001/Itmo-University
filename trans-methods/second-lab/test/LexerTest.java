import org.junit.Test;

import java.util.ArrayList;
import java.util.List;

import static junit.framework.TestCase.assertEquals;

public class LexerTest {

    private static final String X_AND_Y = "x and y";
    private static final String X_AND_Y_WITH_PARENTHESIS = "(x and y)";
    private static final String NOT_X_AND_Y_IN_Z = "not (x and y) in z";
    private static final String MULTIPLE_TOKENS = "(x and      y        or z)      in f";
    private static final String UNDEFINED = "1";

    @Test
    public void xAndY() {
        assertEquals(getTokensFromString(X_AND_Y), List.of(Token.VAR, Token.AND, Token.VAR, Token.END));
    }

    @Test
    public void xAndYWithParenthesis() {
        assertEquals(getTokensFromString(X_AND_Y_WITH_PARENTHESIS), List.of(
                Token.LPAREN,
                Token.VAR,
                Token.AND,
                Token.VAR,
                Token.RPAREN,
                Token.END
        ));
    }

    @Test
    public void notXAndYInZ() {
        assertEquals(getTokensFromString(NOT_X_AND_Y_IN_Z), List.of(
                Token.NOT,
                Token.LPAREN,
                Token.VAR,
                Token.AND,
                Token.VAR,
                Token.RPAREN,
                Token.IN,
                Token.VAR,
                Token.END
        ));
    }

    @Test
    public void multipleTokens() {
        assertEquals(getTokensFromString(MULTIPLE_TOKENS), List.of(
                Token.LPAREN,
                Token.VAR,
                Token.AND,
                Token.VAR,
                Token.OR,
                Token.VAR,
                Token.RPAREN,
                Token.IN,
                Token.VAR,
                Token.END
        ));
    }

    @Test
    public void undefined() {
        assertEquals(getTokensFromString(UNDEFINED), List.of(Token.UNDEFINED, Token.END));
    }

    private List<Token> getTokensFromString(final String s) {
        final List<Token> ans = new ArrayList<>();
        var lexer = new Lexer(s);
        ans.add(lexer.nextToken());
        while (lexer.getCurrentToken() != Token.END) {
            ans.add(lexer.nextToken());
        }
        return ans;
    }
}
