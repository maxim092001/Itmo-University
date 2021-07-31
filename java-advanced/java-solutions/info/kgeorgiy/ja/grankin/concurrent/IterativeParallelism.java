package info.kgeorgiy.ja.grankin.concurrent;

import info.kgeorgiy.java.advanced.concurrent.AdvancedIP;
import info.kgeorgiy.java.advanced.mapper.ParallelMapper;

import java.util.*;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

public class IterativeParallelism implements AdvancedIP {
    private final ParallelMapper parallelMapper;

    public IterativeParallelism(final ParallelMapper parallelMapper) {
        this.parallelMapper = parallelMapper;
    }

    public IterativeParallelism() {
        this.parallelMapper = null;
    }

    private <T, U> U task(
            final int numberOfThreads,
            final List<? extends T> values,
            final Function<Stream<? extends T>, ? extends U> function,
            final Function<Stream<? extends U>, ? extends U> collector
    ) throws InterruptedException {
        return collector.apply(threadsResults(function, splitValuesForTasks(numberOfThreads, values)).stream());
    }

    private <T, U> List<U> threadsResults(
            final Function<Stream<? extends T>, ? extends U> function,
            final List<Stream<? extends T>> splitTasks
    ) throws InterruptedException {
        if (Objects.nonNull(parallelMapper)) {
            return parallelMapper.map(function, splitTasks);
        } else {
            final var res = new ArrayList<U>(Collections.nCopies(splitTasks.size(), null));
            var threads = IntStream
                    .range(0, splitTasks.size())
                    .mapToObj(i -> {
                        var t = new Thread(() -> res.set(i, function.apply(splitTasks.get(i))));
                        t.start();
                        return t;
                    })
                    .collect(Collectors.toList());

            for (final Thread thread : threads) {
                thread.join();
            }
            return res;
        }
    }

    private <T> List<Stream<? extends T>> splitValuesForTasks(
            final int numberOfThreads,
            final List<? extends T> values
    ) {
        final List<Stream<? extends T>> splitParts = new ArrayList<>();
        final int blockSize = values.size() / numberOfThreads;
        int tail = values.size() % numberOfThreads;
        int left = 0;
        for (int i = 0; i < numberOfThreads; i++) {
            final int currentSize = blockSize + (tail > 0 ? 1 : 0);
            tail = Math.max(tail - 1, 0);
            if (currentSize > 0) {
                splitParts.add(values.subList(left, left + currentSize).stream());
            }
            left += currentSize;
        }
        return splitParts;
    }

    @Override
    public String join(final int threads, final List<?> values) throws InterruptedException {
        return task(
                threads,
                values,
                s -> s.map(Object::toString).collect(Collectors.joining()),
                s -> s.collect(Collectors.joining())
        );
    }

    @Override
    public <T> List<T> filter(
            final int threads,
            final List<? extends T> values,
            final Predicate<? super T> predicate
    ) throws InterruptedException {
        return task(
                threads,
                values,
                s -> s.filter(predicate).collect(Collectors.toList()),
                s -> s.flatMap(List::stream).collect(Collectors.toList())
        );
    }

    @Override
    public <T, U> List<U> map(
            final int threads,
            final List<? extends T> values,
            final Function<? super T, ? extends U> f
    ) throws InterruptedException {
        return task(
                threads,
                values,
                s -> s.map(f).collect(Collectors.toList()),
                s -> s.flatMap(List::stream).collect(Collectors.toList())
        );
    }

    @Override
    public <T> T maximum(
            final int threads,
            final List<? extends T> values,
            final Comparator<? super T> comparator
    ) throws InterruptedException {
        return task(
                threads,
                values,
                s -> s.max(comparator).orElse(null),
                s -> s.max(comparator).orElse(null)
        );
    }

    @Override
    public <T> T minimum(
            final int threads,
            final List<? extends T> values,
            final Comparator<? super T> comparator
    ) throws InterruptedException {
        return maximum(threads, values, comparator.reversed());
    }

    @Override
    public <T> boolean all(
            final int threads,
            final List<? extends T> values,
            final Predicate<? super T> predicate
    ) throws InterruptedException {
        return !any(threads, values, predicate.negate());
    }

    @Override
    public <T> boolean any(
            final int threads,
            final List<? extends T> values,
            final Predicate<? super T> predicate
    ) throws InterruptedException {
        return task(
                threads,
                values,
                s -> s.anyMatch(predicate),
                s -> s.anyMatch(Boolean::booleanValue)
        );
    }

    @Override
    public <T> T reduce(
            final int threads,
            final List<T> values,
            final Monoid<T> monoid
    ) throws InterruptedException {
        return task(
                threads,
                values,
                s -> s.reduce(monoid.getIdentity(), monoid.getOperator(), monoid.getOperator()),
                s -> s.reduce(monoid.getIdentity(), monoid.getOperator(), monoid.getOperator())
        );
    }

    @Override
    public <T, R> R mapReduce(
            final int threads,
            final List<T> values,
            final Function<T, R> lift,
            final Monoid<R> monoid
    ) throws InterruptedException {
        var mapped = map(threads, values, lift);
        return reduce(threads, mapped, monoid);
    }
}
