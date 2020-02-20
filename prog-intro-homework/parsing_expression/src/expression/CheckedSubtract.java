package expression;

import exception.OverflowException;

public class CheckedSubtract extends AbstractBinaryOperation {


    public CheckedSubtract(CommonExpression firstExpression, CommonExpression secondExpression) {
        super(firstExpression, secondExpression);
    }

    private boolean isOverflow(int x, int y) {
        return (y > 0 && x < Integer.MIN_VALUE + y) || (y < 0 && x > Integer.MAX_VALUE + y);
    }

    @Override
    protected int operation(int x, int y) {
        if (isOverflow(x, y)) {
            throw new OverflowException("Subtraction overflow: " + x + "-" + y);
        }
        return x - y;
    }
}
