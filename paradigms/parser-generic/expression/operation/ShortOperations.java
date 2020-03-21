package expression.operation;

import expression.exception.DivisionByZeroException;

public class ShortOperations implements Operations<Short> {
    @Override
    public Short add(Short a, Short b) {
        return (short) (a + b);
    }

    @Override
    public Short sub(Short a, Short b) {
        return (short) (a - b);
    }

    @Override
    public Short div(Short a, Short b) {
        if (b == 0) {
            throw new DivisionByZeroException("Division by zero: " + a + "/" + b);
        }
        return (short) (a / b);
    }

    @Override
    public Short mul(Short a, Short b) {
        return (short) (a * b);
    }

    @Override
    public Short min(Short a, Short b) {
        return (short) Integer.min(a, b);
    }

    @Override
    public Short max(Short a, Short b) {
        return (short) Integer.max(a, b);
    }

    @Override
    public Short parseValue(String a) {
        return (short) Integer.parseInt(a);
    }

    @Override
    public Short negate(Short a) {
        return (short) -a;
    }

    @Override
    public Short count(Short a) {
        return (short) Integer.bitCount(Short.toUnsignedInt(a));
    }
}
