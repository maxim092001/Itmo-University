package expression.parser;

public class StringSource implements ExpressionSource {

    private final String source;
    private int index;

    public StringSource(final String source) {
        this.source = source;
        index = 0;
    }


    @Override
    public char next() {
        return source.charAt(index++);
    }

    @Override
    public char peekNext() {
        if (hasNext()) {
            return source.charAt(index);
        } else {
            return '\0';
        }
    }

    @Override
    public boolean hasNext() {
        return index < source.length();
    }

    @Override
    public int getCurrentPosition() {
        return index;
    }
}
