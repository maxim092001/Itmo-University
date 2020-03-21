package expression.parser;

import expression.operation.BinaryOperations;

public class Divide<T> extends AbstractBinaryOperation<T> {

    public Divide(CommonExpression<T> firstExpression, CommonExpression<T> secondExpression,
           BinaryOperations<T> operation) {
        super(firstExpression, secondExpression, operation);
    }

    @Override
    protected T operation(T x, T y) {
        return super.binaryOperation.div(x, y);
    }
}
