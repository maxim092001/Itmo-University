package expression.operation;

public interface BinaryOperations<T> {
    T add(T a, T b);

    T sub(T a, T b);

    T div(T a, T b);

    T mul(T a, T b);

    T min(T a, T b);

    T max(T a, T b);
    T parseValue(String a);

}
