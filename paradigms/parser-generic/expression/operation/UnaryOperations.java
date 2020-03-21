package expression.operation;

public interface UnaryOperations<T> {
    T negate(T a);
    T count(T a);
}
