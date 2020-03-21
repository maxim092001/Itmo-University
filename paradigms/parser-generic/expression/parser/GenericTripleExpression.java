package expression.parser;

public interface GenericTripleExpression<T> {
    T evaluate(T x, T y, T z);
}
