package org.mathoptimization.math_optimization.exception;


public class ApiJsonException extends RuntimeException {
    public ApiJsonException(final String message, Exception e) {
        super(message, e);
    }
}
