package expression.parser;

public interface ExpressionSource {
    char next();
    char peekNext();
    boolean hasNext();
    int getCurrentPosition();
}
