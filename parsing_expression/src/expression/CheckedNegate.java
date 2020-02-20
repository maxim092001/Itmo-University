package expression;

import exception.OverflowException;

public class CheckedNegate extends AbstractUnaryOperation {

    public CheckedNegate(CommonExpression expression) {
        super(expression);
    }

    private boolean isOverflow(int x) {
        return x == Integer.MIN_VALUE;
    }

    @Override
    public int operation(int x) {
        if (isOverflow(x)) {
            throw new OverflowException("Negation overflow: " + x);
        }
        return -1 * x;
    }
}
