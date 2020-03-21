package expression.parser;

import expression.operation.BinaryOperations;

public class Max<T> extends AbstractBinaryOperation<T> {

    public Max(CommonExpression<T> firstExpression, CommonExpression<T> secondExpression,
               BinaryOperations<T> operation) {
        super(firstExpression, secondExpression, operation);
    }

    @Override
    protected T operation(T x, T y) {
        return super.binaryOperation.max(x, y);
    }
}
