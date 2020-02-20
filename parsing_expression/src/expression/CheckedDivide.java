package expression;

import exception.DivisionByZeroException;
import exception.OverflowException;

public class CheckedDivide extends AbstractBinaryOperation {

    public CheckedDivide(CommonExpression firstExpression, CommonExpression secondExpression) {
        super(firstExpression, secondExpression);
    }

    private boolean isOverflow(int x, int y) {
        return x == Integer.MIN_VALUE && y == -1;
    }

    private boolean isDivisionByZero(int x, int y) {
        return y == 0;
    }

    @Override
    protected int operation(int x, int y) {
        if (isOverflow(x, y)) {
            throw new OverflowException("Division overflow: " + x + "/" + y);
        }
        if (isDivisionByZero(x, y)) {
            throw new DivisionByZeroException("Division by zero: " + x + "/" + y);
        }
        return x / y;
    }
}
