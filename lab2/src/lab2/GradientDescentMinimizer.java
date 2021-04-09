package lab2;

import java.util.List;
import java.util.function.Function;

public class GradientDescentMinimizer implements GradientMinimizer {

    public MinPointAndFunction minimize(
            final Function<MultiDimensionalPoint, Double> f,
            final double learningRate,
            final double eps,
            final MultiDimensionalPoint point
    ) {
        final List<Double> gradient = gradient(f, point, eps);
        final double fPoint = f.apply(point);
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
        System.out.println(new GradientDescentMinimizer().minimize(
                p -> {
                    var s = p.getPoints().stream().reduce(0.0, Double::sum);
                    return s * s;
                },
                120,
                0.000000001,
                MultiDimensionalPoint.of(10.0)
        ));
//        Function<MultiDimensionalPoint, Double> f = p -> {
//            var s = p.getPoints().stream().reduce(0.0, Double::sum);
//            return s * s;
//        };
////        System.out.println(f.apply(MultiDimensionalPoint.of(10.0)));
//        System.out.println(new GradientDescentMinimizer().gradient(f, MultiDimensionalPoint.of(10.0), 0.0000001));
    }
}
