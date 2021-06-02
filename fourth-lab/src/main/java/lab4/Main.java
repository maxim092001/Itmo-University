package lab4;

import lab4.matrix.Vector;
import lab4.newton.*;
import lab4.utils.MethodEnum;

import java.util.Arrays;
import java.util.function.Function;

public class Main {
    public static void main(String[] args) {
        var d1 = new ClassicNewtonMethod(v -> 8 * v.get(0) * v.get(0) + 4 * v.get(0) * v.get(1) + 5 * v.get(1) * v.get(1) ,
                1e-4, Vector.of(1d, 2d)).hesseMatrixCalculation(Vector.of(1d, 2d));
        Arrays.stream(d1).forEach(i -> {
            Arrays.stream(i).forEach(el -> System.out.print(el + " "));
            System.out.println();
        });

        var d2 = new ClassicNewtonMethod(v -> Math.sin(v.get(0)) ,
                1e-4, Vector.of(1d)).hesseMatrixCalculation(Vector.of(1d, 2d));
        Arrays.stream(d2).forEach(i -> {
            Arrays.stream(i).forEach(el -> System.out.print(el + " "));
            System.out.println();
        });

        System.out.println(new ClassicNewtonMethod(v -> 8 * v.get(0) * v.get(0) + 4 * v.get(0) * v.get(1) + 5 * v.get(1) * v.get(1) ,
                1e-4, Vector.of(1d, 2d)).minimize());

        System.out.println(new ClassicNewtonMethod(v -> Math.sin(v.get(0)) ,
                1e-4, Vector.of(1d)).minimize());


        System.out.println(new OneDirectionNewtonMethod(v -> 8 * v.get(0) * v.get(0) + 4 * v.get(0) * v.get(1) + 5 * v.get(1) * v.get(1) ,
                1e-4, Vector.of(1d, 2d)).minimize());

        System.out.println(new OneDirectionNewtonMethod(v -> Math.sin(v.get(0)) ,
                1e-4, Vector.of(1d)).minimize());


        System.out.println(new DescendMethod(v -> 8 * v.get(0) * v.get(0) + 4 * v.get(0) * v.get(1) + 5 * v.get(1) * v.get(1) ,
                1e-4, Vector.of(1d, 2d)).minimize());

        System.out.println(new DescendMethod(v -> Math.sin(v.get(0)) ,
                1e-4, Vector.of(1d)).minimize());

        System.out.println(new DavidonFletcherPowellMethod(v -> 8 * v.get(0) * v.get(0) + 4 * v.get(0) * v.get(1) + 5 * v.get(1) * v.get(1) ,
                1e-4, Vector.of(1d, 2d)).minimize());

        System.out.println(new DavidonFletcherPowellMethod(v -> Math.sin(v.get(0)) ,
                1e-4, Vector.of(1d)).minimize());

        System.out.println(new DavidonFletcherPowellMethod(
                v -> 100 * (v.get(1) - v.get(0) * v.get(0)) *  (v.get(1) - v.get(0) * v.get(0)) + (1 - v.get(0)) * (1 - v.get(0)),
                1e-4, Vector.of(0d, 0d)).minimize());

        System.out.println(new PowellMethod(v -> 8 * v.get(0) * v.get(0) + 4 * v.get(0) * v.get(1) + 5 * v.get(1) * v.get(1) ,
                1e-4, Vector.of(1d, 2d)).minimize());

        System.out.println(new PowellMethod(v -> Math.sin(v.get(0)) ,
                1e-4, Vector.of(1d)).minimize());

        System.out.println(new PowellMethod(
                v -> 100 * (v.get(1) - v.get(0) * v.get(0)) *  (v.get(1) - v.get(0) * v.get(0)) + (1 - v.get(0)) * (1 - v.get(0)),
                1e-4, Vector.of(0d, 0d)).minimize());

        System.out.println("Tables:");
        generateTables(MethodEnum.CLASSIC_NEWTON, v -> 8 * v.get(0) * v.get(0) + 4 * v.get(0) * v.get(1) + 5 * v.get(1) * v.get(1), Vector.of(1d, 2d));
        generateTables(MethodEnum.ONE_DIRECTION_NEWTON, v -> 8 * v.get(0) * v.get(0) + 4 * v.get(0) * v.get(1) + 5 * v.get(1) * v.get(1), Vector.of(1d, 2d));
        generateTables(MethodEnum.DESCEND_NEWTON, v -> 8 * v.get(0) * v.get(0) + 4 * v.get(0) * v.get(1) + 5 * v.get(1) * v.get(1), Vector.of(1d, 2d));
    }

    private static void generateTables(final MethodEnum method, final Function<Vector, Double> function, final Vector startPoint) {
        final AbstractNewtonMethod optimizationMethod = switch (method) {
                    case CLASSIC_NEWTON -> new ClassicNewtonMethod(function, 1e-4, startPoint);
                    case ONE_DIRECTION_NEWTON -> new OneDirectionNewtonMethod(function, 1e-4, startPoint);
                    case DESCEND_NEWTON -> new DescendMethod(function, 1e-4, startPoint);
                };
        optimizationMethod.minimize();
        System.out.println(optimizationMethod.getSteps());
    }
}
