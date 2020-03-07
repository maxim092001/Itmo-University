package expression;

public abstract class AbstractUnaryOperation implements CommonExpression {
    private CommonExpression expression;

    AbstractUnaryOperation(CommonExpression expression) {
        this.expression = expression;
    }


    @Override
    public int evaluate(int x) {
        return operation(expression.evaluate(x));
    }

    @Override
    public int evaluate(int x, int y, int z) {
        return operation(expression.evaluate(x, y, z));
    }

    public abstract int operation(int x);
}
