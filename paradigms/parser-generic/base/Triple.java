package base;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class Triple<F, S, T> {
    private final F first;
    private final S second;
    private final T third;

    public Triple(final F first, final S second, final T third) {
        this.first = first;
        this.second = second;
        this.third = third;
    }

    public F first() {
        return first;
    }

    public S second() {
        return second;
    }

    public T third() {
        return third;
    }

    public static <F, S, T> Triple<F, S, T> of(final F first, final S second, final T third) {
        return new Triple<>(first, second, third);
    }
}
