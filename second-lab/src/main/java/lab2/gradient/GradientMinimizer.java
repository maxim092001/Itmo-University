package lab2.gradient;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

/**
 * Common interface for gradient calculations.
 */
public interface GradientMinimizer {

    /**
     * Function to calculate the gradient for a given function and point.
     *
     * @param f      specified function
     * @param point  specified point
     * @param fPoint function value at point
     * @param eps    epsilon
     * @return a list of partial derivatives at point.
     */
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

    /**
     * Function for calculating the rate of a given gradient.
     *
     * @param gradient list of partial derivatives
     * @return the norm of the given gradient.
     */
    default double gradientRate(final List<Double> gradient) {
        return Math.sqrt(gradient.stream().reduce(0.0, (sum, el) -> sum + el * el));
    }
}
