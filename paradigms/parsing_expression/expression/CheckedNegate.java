package expression;

import exception.OverflowException;

public class CheckedNegate extends AbstractUnaryOperation {

    public CheckedNegate(CommonExpression expression) {
        super(expression);
    }

    private void checkOverflow(int x) {
        if (x == Integer.MIN_VALUE) {
            throw new OverflowException("Negation overflow: " + x);
        }
    }

    @Override
    public int operation(int x) {
        checkOverflow(x);
        return -1 * x;
    }
}
