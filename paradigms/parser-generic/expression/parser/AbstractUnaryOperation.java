package expression.parser;

import expression.operation.UnaryOperations;

public abstract class AbstractUnaryOperation<T> implements CommonExpression<T> {
    private CommonExpression<T> expression;
    protected UnaryOperations<T> unaryOperation;

    AbstractUnaryOperation(CommonExpression<T> expression, UnaryOperations<T> unaryOperation) {
        this.expression = expression;
        this.unaryOperation = unaryOperation;
    }


    @Override
    public T evaluate(T x) {
        return operation(expression.evaluate(x));
    }

    @Override
    public T evaluate(T x, T y, T z) {
        return operation(expression.evaluate(x, y, z));
    }

    public abstract T operation(T x);
}
