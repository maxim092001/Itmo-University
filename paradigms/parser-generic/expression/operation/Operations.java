package expression.operation;

public interface Operations<T> extends BinaryOperations<T>, UnaryOperations<T> {
    T parseValue(String value);
}
