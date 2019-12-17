package expression;

public interface ToMiniString {
    default String toMiniString() {
        return toString();
    }
}
