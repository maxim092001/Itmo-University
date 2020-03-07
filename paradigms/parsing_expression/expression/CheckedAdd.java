package expression;

import exception.OverflowException;

public class CheckedAdd extends AbstractBinaryOperation {

    public CheckedAdd(CommonExpression firstExpression, CommonExpression secondExpression) {
        super(firstExpression, secondExpression);
    }

    private void checkOverflow(int x, int y) {
        if ((y > 0 && x > Integer.MAX_VALUE - y) || (y < 0 && x < Integer.MIN_VALUE - y)) {
            throw new OverflowException("Addition overflow: " + x + "+" + y);
        }
    }

    @Override
    protected int operation(int x, int y) {
        checkOverflow(x, y);
        return x + y;
    }
}
