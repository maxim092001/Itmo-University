package expression.exception;

public class FormatException extends ParsingException {
    public FormatException(String message, int errorOffset) {
        super(message, errorOffset);
    }
}
