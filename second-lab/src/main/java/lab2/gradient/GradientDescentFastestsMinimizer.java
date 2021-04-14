//package lab2.gradient;
//
//import lab2.methods.FibonacciMethod;
//
//import java.util.List;
//import java.util.function.Function;
//
//public class GradientDescentFastestsMinimizer extends GradientDescentMinimizer {
//    public MinPointAndFunction minimize(
//            final Function<MultiDimensionalPoint, Double> f,
//            final double eps,
//            final MultiDimensionalPoint point,
//            final double maxEigenvalue
//    ) {
//        final double fPoint = f.apply(point);
//        final List<Double> gradient = gradient(f, point, fPoint, eps);
//        if (gradientRate(gradient) < eps) {
//            return MinPointAndFunction.of(point, fPoint);
//        } else {
//            return newPoint(f, eps, point, fPoint, gradient, maxEigenvalue);
//        }
//    }
//
//    private MinPointAndFunction newPoint(
//            final Function<MultiDimensionalPoint, Double> f,
//            final double eps,
//            final MultiDimensionalPoint point,
//            final double fPoint,
//            final List<Double> gradient,
//            final double maxEigenvalue
//    ) {
//        final var method = new FibonacciMethod(
//                0.0,
//                maxEigenvalue,
//                alpha ->
//                f.apply(
//                        point.sub(
//                                MultiDimensionalPoint.of(gradient(f, point, fPoint, eps)).mul(alpha)
//                        )
//                ),
//                eps
//        );
//        method.calculate();
//        final double learningRate = method.getMinimumArgument();
//        final MultiDimensionalPoint y = point.sub(MultiDimensionalPoint.of(gradient).mul(learningRate));
//        final double fY = f.apply(y);
//        if (fY < fPoint) {
//            return minimize(f, eps, y, maxEigenvalue);
//        } else {
//            return newPoint(f, eps, point, fPoint, gradient, maxEigenvalue);
//        }
//    }
//
//    public static void main(String[] args) {
//        // l , L --> 2 / (l + L)
//        System.out.println(new GradientDescentFastestsMinimizer().minimize(
//                p -> {
//                    var s = p.getPoints().stream().reduce(0.0, Double::sum);
//                    return s * s;
//                },
//                0.000000001,
//                MultiDimensionalPoint.of(10.0), // n = 1, x1 = 10.0
//            1 // собственное число
//        ));
//    }
//
//}
