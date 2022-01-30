public class Lexer {

    private final StringWrapper wrapper;
    private Token currentToken;

    public Lexer(final String s) {
        this.wrapper = StringWrapper.of(s);
    }

    public Token nextToken() {
        skipWhitespaces();
        final char c = wrapper.next();
        return currentToken = switch (c) {
            case '(' -> Token.LPAREN;
            case ')' -> Token.RPAREN;
            default -> parseIdentifier(c);
        };
    }

    private Token parseIdentifier(char last) {
        final var sb = new StringBuilder();
        sb.append(last);
        while (wrapper.hasNext()) {
            final char cur = wrapper.peek();
            if (isWhitespace(cur)) {
                break;
            }
            if (isParenthesis(cur)) {
                break;
            }
            sb.append(wrapper.next());
        }

        final var curString = sb.toString();
        return Token.getTokenByString(curString);
    }

    private static boolean isParenthesis(final char c) {
        return c == ')' || c == '(';
    }

    private void skipWhitespaces() {
        while (wrapper.hasNext() && isWhitespace(wrapper.peek())) {
            wrapper.next();
        }
    }

    private boolean isWhitespace(final char c) {
        return Character.isWhitespace(c);
    }

    private static class StringWrapper {

        private static final char END = '\0';

        private final String s;
        private int curIndex = 0;

        private StringWrapper(final String s) {
            this.s = s;
        }

        public static StringWrapper of(final String s) {
            return new StringWrapper(s);
        }

        public char next() {
            return getByIdx(curIndex++);
        }

        public boolean hasNext() {
            return getByIdx(curIndex + 1) != END;
        }

        public char peek() {
            return getByIdx(curIndex);
        }

        public char peekNext() {
            return getByIdx(curIndex + 1);
        }

        private char getByIdx(final int idx) {
            return idx >= 0 && idx < s.length() ? s.charAt(idx) : END;
        }
    }

    public Token getCurrentToken() {
        return currentToken;
    }

    public boolean isEmpty() {
        return currentToken == Token.END;
    }

    public int getCurrentIndex() {
        return this.wrapper.curIndex;
    }
}
