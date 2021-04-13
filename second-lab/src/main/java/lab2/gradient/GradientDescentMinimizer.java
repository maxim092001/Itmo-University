package lab2.gradient;

import java.util.List;
import java.util.function.Function;

public class GradientDescentMinimizer implements GradientMinimizer {

    public MinPointAndFunction minimize(
            final Function<MultiDimensionalPoint, Double> f,
            final double learningRate,
            final double eps,
            final MultiDimensionalPoint point
    ) {
        final double fPoint = f.apply(point);
        final List<Double> gradient = gradient(f, point, fPoint, eps);
        if (gradientRate(gradient) < eps) {
            return MinPointAndFunction.of(point, fPoint);
        } else {
            return newPoint(f, learningRate, eps, point, fPoint, gradient);
        }
    }

    private MinPointAndFunction newPoint(
            final Function<MultiDimensionalPoint, Double> f,
            final double learningRate,
            final double eps,
            final MultiDimensionalPoint point,
            final double fPoint,
            final List<Double> gradient
    ) {
        final MultiDimensionalPoint y = point.sub(MultiDimensionalPoint.of(gradient).mul(learningRate));
        final double fY = f.apply(y);
        if (fY < fPoint) {
            return minimize(f, learningRate, eps, y);
        } else {
            return newPoint(f, learningRate / 2, eps, point, fPoint, gradient);
        }
    }

    public static void main(String[] args) {
        // l , L --> 2 / (l + L)
        System.out.println(new GradientDescentMinimizer().minimize(
                p -> {
                    var s = p.getPoints().stream().reduce(0.0, Double::sum);
                    return s * s;
                },
                120,
                0.000000001,
                MultiDimensionalPoint.of(10.0) // n = 1, x1 = 10.0
        ));
    }
}
