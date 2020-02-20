package exception;

import java.text.ParseException;

public class ParsingException extends ParseException {
    public ParsingException(String message, int errorOffset) {
        super(message + " at index " + errorOffset, errorOffset);
    }
}
