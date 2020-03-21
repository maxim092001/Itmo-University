package expression.parser;

import expression.operation.BinaryOperations;

public abstract class AbstractBinaryOperation<T> implements CommonExpression<T> {
    private CommonExpression<T> firstExpression, secondExpression;
    protected BinaryOperations<T> binaryOperation;

    AbstractBinaryOperation(CommonExpression<T> firstExpression, CommonExpression<T> secondExpression,
                            BinaryOperations<T> binaryOperation) {
        this.firstExpression = firstExpression;
        this.secondExpression = secondExpression;
        this.binaryOperation = binaryOperation;
    }

    @Override
    public T evaluate(T x) {
        return operation(firstExpression.evaluate(x), secondExpression.evaluate(x));
    }

    @Override
    public T evaluate(T x, T y, T z) {
        return operation(firstExpression.evaluate(x, y, z), secondExpression.evaluate(x, y, z));
    }

    protected abstract T operation(T x, T y);
}
