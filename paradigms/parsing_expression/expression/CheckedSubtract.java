package expression;

import exception.OverflowException;

public class CheckedSubtract extends AbstractBinaryOperation {


    public CheckedSubtract(CommonExpression firstExpression, CommonExpression secondExpression) {
        super(firstExpression, secondExpression);
    }

    private void checkOverflow(int x, int y) {
        if ((y > 0 && x < Integer.MIN_VALUE + y) || (y < 0 && x > Integer.MAX_VALUE + y)) {
            throw new OverflowException("Subtraction overflow: " + x + "-" + y);
        }
    }

    @Override
    protected int operation(int x, int y) {
        checkOverflow(x, y);
        return x - y;
    }
}
