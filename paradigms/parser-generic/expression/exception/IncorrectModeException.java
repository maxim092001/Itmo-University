package expression.exception;

public class IncorrectModeException extends Exception {
    public IncorrectModeException(String mode) {
        super("Incorrect mode: " + mode);
    }
}
