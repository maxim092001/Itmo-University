package expression.parser;

import java.util.function.Function;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public interface Either<L, R> {
    <NR> Either<L, NR> flatMapRight(final Function<? super R, ? extends Either<L, NR>> f);

    boolean isRight();

    L getLeft();
    R getRight();

    static <L, R> Either<L, R> right(final R value) {
        return new Either<L, R>() {

            @Override
            public <NR> Either<L, NR> flatMapRight(final Function<? super R, ? extends Either<L, NR>> f) {
                return f.apply(value);
            }

            @Override
            public boolean isRight() {
                return true;
            }

            @Override
            public L getLeft() {
                return null;
            }

            @Override
            public R getRight() {
                return value;
            }
        };
    }

    static <L, R> Either<L, R> left(final L value) {
        return new Either<L, R>() {

            @Override
            public <NR> Either<L, NR> flatMapRight(final Function<? super R, ? extends Either<L, NR>> f) {
                return left(value);
            }

            @Override
            public boolean isRight() {
                return false;
            }

            @Override
            public L getLeft() {
                return value;
            }

            @Override
            public R getRight() {
                return null;
            }
        };
    }
}
