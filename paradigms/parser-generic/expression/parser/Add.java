package expression.parser;

import expression.operation.BinaryOperations;

public class Add<T> extends AbstractBinaryOperation<T> {

    public Add(CommonExpression<T> firstExpression, CommonExpression<T> secondExpression,
               BinaryOperations<T> operation) {
        super(firstExpression, secondExpression, operation);
    }

    @Override
    protected T operation(T x, T y) {
        return super.binaryOperation.add(x, y);
    }
}
