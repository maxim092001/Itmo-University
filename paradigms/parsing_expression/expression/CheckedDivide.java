package expression;

import exception.DivisionByZeroException;
import exception.OverflowException;

public class CheckedDivide extends AbstractBinaryOperation {

    public CheckedDivide(CommonExpression firstExpression, CommonExpression secondExpression) {
        super(firstExpression, secondExpression);
    }

    private void checkOverflow(int x, int y) {
        if (x == Integer.MIN_VALUE && y == -1) {
            throw new OverflowException("Division overflow: " + x + "/" + y);
        }
    }

    private void checkDivisionByZero(int x, int y) {
        if (y == 0) {
            throw new DivisionByZeroException("Division by zero: " + x + "/" + y);
        }
    }

    @Override
    protected int operation(int x, int y) {
        checkOverflow(x, y);
        checkDivisionByZero(x, y);
        return x / y;
    }
}
