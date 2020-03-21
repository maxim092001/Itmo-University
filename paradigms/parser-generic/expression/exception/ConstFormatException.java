package expression.exception;

public class ConstFormatException extends ParsingException {
    public ConstFormatException(String message, int errorOffset) {
        super(message, errorOffset);
    }
}
