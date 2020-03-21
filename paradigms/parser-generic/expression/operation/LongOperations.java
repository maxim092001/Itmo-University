package expression.operation;

import expression.exception.DivisionByZeroException;

public class LongOperations implements Operations<Long> {
    @Override
    public Long add(Long a, Long b) {
        return a + b;
    }

    @Override
    public Long sub(Long a, Long b) {
        return a - b;
    }

    @Override
    public Long div(Long a, Long b) {
        if (b == 0) {
            throw new DivisionByZeroException("Division by zero: " + a + "/" + b);
        }
        return a / b;
    }

    @Override
    public Long mul(Long a, Long b) {
        return a * b;
    }

    @Override
    public Long min(Long a, Long b) {
        return Long.min(a, b);
    }

    @Override
    public Long max(Long a, Long b) {
        return Long.max(a, b);
    }

    @Override
    public Long parseValue(String value) {
        return Long.parseLong(value);
    }

    @Override
    public Long negate(Long a) {
        return -a;
    }

    @Override
    public Long count(Long a) {
        return (long) Long.bitCount(a);
    }
}
