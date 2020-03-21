package expression.parser;

import expression.operation.UnaryOperations;

public class Count<T> extends AbstractUnaryOperation<T> {
    public Count(CommonExpression<T> expression, UnaryOperations<T> unaryOperation) {
        super(expression, unaryOperation);
    }

    @Override
    public T operation(T x) {
        return super.unaryOperation.count(x);
    }
}
