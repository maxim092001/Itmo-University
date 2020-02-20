package exception;


public class IllegalOperation extends ParsingException {
    public IllegalOperation(String message, int errorOffset) {
        super(message, errorOffset);
    }
}
