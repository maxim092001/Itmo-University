package expression.parser;

import expression.operation.BinaryOperations;

public class Multiply<T> extends AbstractBinaryOperation<T> {

    public Multiply(CommonExpression<T> firstExpression, CommonExpression<T> secondExpression, BinaryOperations<T> binaryOperation) {
        super(firstExpression, secondExpression, binaryOperation);
    }

    @Override
    protected T operation(T x, T y) {
        return binaryOperation.mul(x, y);
    }
}
