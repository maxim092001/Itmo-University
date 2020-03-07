package expression;

import exception.OverflowException;

public class CheckedMultiply extends AbstractBinaryOperation {

    public CheckedMultiply(CommonExpression firstExpression, CommonExpression secondExpression) {
        super(firstExpression, secondExpression);
    }


    public static int checkOverflow(int x, int y) {
        int maximum = Integer.signum(x) == Integer.signum(y) ? Integer.MAX_VALUE : Integer.MIN_VALUE;

        if ((x == -1 && y == Integer.MIN_VALUE)
                || (x != -1 && x != 0 && ((y > 0 && y > maximum / x)
                || (y < 0 && y < maximum / x )))) {
            throw new OverflowException("Multiplication overflow: " + x + "*" + y);
        }
        return x * y;
    }

    @Override
    protected int operation(int x, int y) {
        return checkOverflow(x, y);
    }
}
