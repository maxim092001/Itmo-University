package exception;

public class DivisionByZeroException extends EvaluatingException {
    public DivisionByZeroException(String message) {
        super(message);
    }
}
