package expression.parser;

import expression.operation.BinaryOperations;

public class Min<T> extends AbstractBinaryOperation<T> {

    public Min(CommonExpression<T> firstExpression, CommonExpression<T> secondExpression,
               BinaryOperations<T> operation) {
        super(firstExpression, secondExpression, operation);
    }

    @Override
    protected T operation(T x, T y) {
        return super.binaryOperation.min(x, y);
    }
}
