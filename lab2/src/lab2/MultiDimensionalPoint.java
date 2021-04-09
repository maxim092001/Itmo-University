package lab2;

import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class MultiDimensionalPoint {
    private int dimension;
    private List<Double> points;

    private MultiDimensionalPoint(final List<Double> points) {
        this.dimension = points.size();
        this.points = points;
    }

    public static MultiDimensionalPoint of(final List<Double> points) {
        return new MultiDimensionalPoint(points);
    }
    
    public static MultiDimensionalPoint of(Double ...args) {
        return new MultiDimensionalPoint(List.of(args));
    }

    public MultiDimensionalPoint sub(final MultiDimensionalPoint that) {
        if (points.size() != that.points.size()) {
            throw new IllegalArgumentException("Different point sizes");
        }
        return MultiDimensionalPoint.of(
                IntStream.range(0, points.size())
                .mapToObj(i -> points.get(i) - that.points.get(i))
                        .collect(Collectors.toList())
        );
    }

    public MultiDimensionalPoint mul(final double value) {
        return MultiDimensionalPoint.of(points.stream().map(el -> el * value).collect(Collectors.toList()));
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        final MultiDimensionalPoint that = (MultiDimensionalPoint) o;
        return dimension == that.dimension && Objects.equals(points, that.points);
    }

    @Override
    public int hashCode() {
        return Objects.hash(dimension, points);
    }

    public int getDimension() {
        return dimension;
    }

    public void setDimension(final int dimension) {
        this.dimension = dimension;
    }

    public List<Double> getPoints() {
        return points;
    }

    public void setPoints(final List<Double> points) {
        this.points = points;
    }
}
