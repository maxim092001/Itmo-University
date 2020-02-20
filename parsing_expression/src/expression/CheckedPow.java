package expression;


public class CheckedPow extends AbstractBinaryOperation {

    public CheckedPow(CommonExpression firstExpression, CommonExpression secondExpression) {
        super(firstExpression, secondExpression);
    }


    private boolean isOverflow(int x, int y) {
        return CheckedMultiply.isOverflow(x, y);
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
        if (y == 0) {
            return 1;
        }

        int result = 1;

        for (int i = 0; i < y; ++i) {
            if (isOverflow(result, x)) {
                throw new ArithmeticException("Exponential overflow ");
            }
            result *= x;

        }
        return result;
    }
}
