package expression.exception;

public class ParenthesisException extends ParsingException {
    public ParenthesisException(String message, int errorOffset) {
        super(message, errorOffset);
    }
}
