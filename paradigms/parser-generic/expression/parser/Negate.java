package expression.parser;

import expression.operation.UnaryOperations;

public class Negate<T> extends AbstractUnaryOperation<T> {

    public Negate(CommonExpression<T> expression, UnaryOperations<T> unaryOperation) {
        super(expression, unaryOperation);
    }

    @Override
    public T operation(T x) {
        return unaryOperation.negate(x);
    }
}
