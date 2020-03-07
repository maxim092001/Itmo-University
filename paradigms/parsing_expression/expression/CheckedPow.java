package expression;


import exception.OverflowException;

public class CheckedPow extends AbstractBinaryOperation {

    public CheckedPow(CommonExpression firstExpression, CommonExpression secondExpression) {
        super(firstExpression, secondExpression);
    }

    @Override
    protected int operation(int x, int y) {

        if (x == 0 && y == 0) {
            throw new ArithmeticException("Two zeros prohibited");
        }

        if (y < 0) {
            throw new ArithmeticException("Negative powers prohibited");
        }

        if (x == 0) {
            return 0;
        }

        int result = 1;

        while (y > 0) {
            try {
                if (y % 2 == 1) {
                    result = CheckedMultiply.checkOverflow(result, x);
                    y--;
                } else {
                    x = CheckedMultiply.checkOverflow(x, x);
                    y >>= 1;
                }
            } catch (OverflowException e) {
                throw new OverflowException("Exponential overflow " + x + "**" + y);
            }
        }

        return result;
    }
}
