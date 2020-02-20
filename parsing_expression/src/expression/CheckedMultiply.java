package expression;

import exception.OverflowException;

public class CheckedMultiply extends AbstractBinaryOperation {

    public CheckedMultiply(CommonExpression firstExpression, CommonExpression secondExpression) {
        super(firstExpression, secondExpression);
    }


    public static boolean isOverflow(int x, int y) {
        int maximum = Integer.signum(x) == Integer.signum(y) ? Integer.MAX_VALUE : Integer.MIN_VALUE;

        return (x == -1 && y == Integer.MIN_VALUE)
                || (x != -1 && x != 0 && ((y > 0 && y > maximum / x)
                || (y < 0 && y < maximum / x )));
    }

    @Override
    protected int operation(int x, int y) {
        if (isOverflow(x, y)) {
            throw new OverflowException("Multiplication overflow: " + x + "*" + y);
        }
        return x * y;
    }
}
