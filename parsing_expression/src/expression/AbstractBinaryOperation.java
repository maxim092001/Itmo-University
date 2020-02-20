package expression;


public abstract class AbstractBinaryOperation implements CommonExpression {
    private CommonExpression firstExpression, secondExpression;

    AbstractBinaryOperation(CommonExpression firstExpression, CommonExpression secondExpression) {
        this.firstExpression = firstExpression;
        this.secondExpression = secondExpression;
    }

    @Override
    public int evaluate(int x) {
        return operation(firstExpression.evaluate(x), secondExpression.evaluate(x));
    }

    @Override
    public int evaluate(int x, int y, int z) {
        return operation(firstExpression.evaluate(x, y, z), secondExpression.evaluate(x, y, z));
    }

    protected abstract int operation(int x, int y);
}
