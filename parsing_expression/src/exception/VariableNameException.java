package exception;

public class VariableNameException extends ParsingException {
    public VariableNameException(String s, int errorOffset) {
        super(s, errorOffset);
    }
}
