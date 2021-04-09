package lab2;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public interface GradientMinimizer {

    default List<Double> gradient(
            final Function<MultiDimensionalPoint, Double> f,
            final MultiDimensionalPoint point,
            final double fPoint,
            final double eps
    ) {
        return IntStream.range(0, point.getPoints().size()).mapToObj(index -> {
            final var newList = new ArrayList<>(point.getPoints());
            newList.set(index, point.getPoints().get(index) + eps);
            return (f.apply(MultiDimensionalPoint.of(newList)) - fPoint) / eps;
        }).collect(Collectors.toList());
    }

    default double gradientRate(final List<Double> gradient) {
        return Math.sqrt(gradient.stream().reduce(0.0, (sum, el) -> sum + el * el));
    }
}
