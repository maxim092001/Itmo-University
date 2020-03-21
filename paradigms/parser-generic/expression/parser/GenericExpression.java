package expression.parser;

public interface GenericExpression<T> {
    T evaluate(T x);
}
