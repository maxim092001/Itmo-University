package expression;

public class CheckedLog extends AbstractBinaryOperation {

    public CheckedLog(CommonExpression firstExpression, CommonExpression secondExpression) {
        super(firstExpression, secondExpression);
    }

    private void checkLogarithmParameters(int x, int y) {
        if (y <= 1 || x <= 0) {
            throw new ArithmeticException("Unacceptable parameters for the logarithm");
        }
    }
    @Override
    protected int operation(int x, int y) {

        checkLogarithmParameters(x, y);
        int pow = 0;

        while (x / y >= 1) {
            x /= y;
            pow++;
        }
        return pow;
    }
}
