package expression.parser;

import expression.operation.BinaryOperations;

public class Subtract<T> extends AbstractBinaryOperation<T> {

    public Subtract(CommonExpression<T> firstExpression, CommonExpression<T> secondExpression, BinaryOperations<T> binaryOperation) {
        super(firstExpression, secondExpression, binaryOperation);
    }

    @Override
    protected T operation(T x, T y) {
        return binaryOperation.sub(x, y);
    }
}
