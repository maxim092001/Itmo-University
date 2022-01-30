import java.util.Map;
import java.util.regex.Pattern;

public enum Token {
    NOT,
    IN,
    OR,
    AND,
    XOR,
    VAR,
    LPAREN,
    RPAREN,
    END,
    UNDEFINED;

    private static final Pattern VARIABLE_MATCHER = Pattern.compile("[a-z]");

    private static final Map<String, Token> allowedStringTokens = Map.of(
            "not", Token.NOT,
            "and", Token.AND,
            "or", Token.OR,
            "xor", Token.XOR,
            "in", Token.IN
    );

    public static Token getTokenByString(final String s) {
        return allowedStringTokens.getOrDefault(s,
                VARIABLE_MATCHER.matcher(s).matches() ?
                        Token.VAR :
                        s.equals("\0") ?
                                Token.END :
                                Token.UNDEFINED
        );
    }
}
