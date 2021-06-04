package lab4;

import lab4.matrix.Vector;
import lab4.newton.*;
import lab4.utils.MethodEnum;

import java.util.Arrays;
import java.util.List;
import java.util.function.Function;

public class Main {
    public static void main(String[] args) {
//        var d1 = new ClassicNewtonMethod(v -> 8 * v.get(0) * v.get(0) + 4 * v.get(0) * v.get(1) + 5 * v.get(1) * v.get(1) ,
//                1e-5, Vector.of(1d, 2d)).hesseMatrixCalculation(Vector.of(1d, 2d));
//        Arrays.stream(d1).forEach(i -> {
//            Arrays.stream(i).forEach(el -> System.out.print(el + " "));
//            System.out.println();
//        });
//
//        var d2 = new ClassicNewtonMethod(v -> Math.sin(v.get(0)) ,
//                1e-5, Vector.of(1d)).hesseMatrixCalculation(Vector.of(1d, 2d));
//        Arrays.stream(d2).forEach(i -> {
//            Arrays.stream(i).forEach(el -> System.out.print(el + " "));
//            System.out.println();
//        });
//
//        System.out.println(new ClassicNewtonMethod(v -> 8 * v.get(0) * v.get(0) + 4 * v.get(0) * v.get(1) + 5 * v.get(1) * v.get(1) ,
//                1e-5, Vector.of(1d, 2d)).minimize());
//
//        System.out.println(new ClassicNewtonMethod(v -> Math.sin(v.get(0)) ,
//                1e-5, Vector.of(1d)).minimize());
//
//
//        System.out.println(new OneDirectionNewtonMethod(v -> 8 * v.get(0) * v.get(0) + 4 * v.get(0) * v.get(1) + 5 * v.get(1) * v.get(1) ,
//                1e-5, Vector.of(1d, 2d)).minimize());
//
//        System.out.println(new OneDirectionNewtonMethod(v -> Math.sin(v.get(0)) ,
//                1e-5, Vector.of(1d)).minimize());
//
//
//        System.out.println(new DescendMethod(v -> 8 * v.get(0) * v.get(0) + 4 * v.get(0) * v.get(1) + 5 * v.get(1) * v.get(1) ,
//                1e-5, Vector.of(1d, 2d)).minimize());
//
//        System.out.println(new DescendMethod(v -> Math.sin(v.get(0)) ,
//                1e-5, Vector.of(1d)).minimize());
//
//        System.out.println(new DavidonFletcherPowellMethod(v -> 8 * v.get(0) * v.get(0) + 4 * v.get(0) * v.get(1) + 5 * v.get(1) * v.get(1) ,
//                1e-5, Vector.of(1d, 2d)).minimize());
//
//        System.out.println(new DavidonFletcherPowellMethod(v -> Math.sin(v.get(0)) ,
//                1e-5, Vector.of(1d)).minimize());
//
//        System.out.println(new DavidonFletcherPowellMethod(
//                v -> 100 * (v.get(1) - v.get(0) * v.get(0)) *  (v.get(1) - v.get(0) * v.get(0)) + (1 - v.get(0)) * (1 - v.get(0)),
//                1e-5, Vector.of(0d, 0d)).minimize());
//
//        System.out.println(new PowellMethod(v -> 8 * v.get(0) * v.get(0) + 4 * v.get(0) * v.get(1) + 5 * v.get(1) * v.get(1) ,
//                1e-5, Vector.of(1d, 2d)).minimize());
//
//        System.out.println(new PowellMethod(v -> Math.sin(v.get(0)) ,
//                1e-5, Vector.of(1d)).minimize());
//
//        System.out.println(new PowellMethod(
//                v -> 100 * (v.get(1) - v.get(0) * v.get(0)) *  (v.get(1) - v.get(0) * v.get(0)) + (1 - v.get(0)) * (1 - v.get(0)),
//                1e-5, Vector.of(0d, 0d)).minimize());
//
//        System.out.println("Tables:");
//        generateTables(MethodEnum.CLASSIC_NEWTON, v -> 8 * v.get(0) * v.get(0) + 4 * v.get(0) * v.get(1) + 5 * v.get(1) * v.get(1), Vector.of(2.0, 1.0));
//        generateTables(MethodEnum.ONE_DIRECTION_NEWTON, v -> 8 * v.get(0) * v.get(0) + 4 * v.get(0) * v.get(1) + 5 * v.get(1) * v.get(1), Vector.of(2.0, 1.0));
//        generateTables(MethodEnum.DESCEND_NEWTON, v -> 8 * v.get(0) * v.get(0) + 4 * v.get(0) * v.get(1) + 5 * v.get(1) * v.get(1), Vector.of(2.0, 1.0));

//        onePointTwo();
        final Function<Vector, Double> f1 = v -> {
            double x1 = v.get(0);
            double x2 = v.get(1);
            return 100.0 * (x2 - x1 * x1) * (x2 - x1 * x1) + (1 - x1) * (1 - x1);
        };
        final Function<Vector, Double> f2 = v -> {
            double x1 = v.get(0);
            double x2 = v.get(1);
            return (x1 * x1 + x2 - 11) * (x1 * x1 + x2 - 11) + (x1 + x2 * x2 - 7) * (x1 + x2 * x2 - 7);
        };
        final Function<Vector, Double> f3 = v -> {
            double x1 = v.get(0);
            double x2 = v.get(1);
            return 100 - 2.0 / (1 + ((x1 - 1) / 2.0) * ((x1 - 1) / 2.0) + ((x2 - 1) / 3.0) * ((x2 - 1) / 3.0)) - 1.0 / (1 + ((x1 - 2) / 2.0) * ((x1 - 2) / 2.0) + ((x2 - 1) / 3.0) * ((x2 - 1) / 3.0));
        };

        final Function<Vector, Double> f4 = v -> {
            double x1 = v.get(0);
            double x2 = v.get(1);
            double x3 = v.get(2);
            double x4 = v.get(3);
            return (x1 + 10 * x2) * (x1 + 10 * x2) + 5 * (x3 - x4) * (x3 - x4) + (x2 - 2 * x3) * (x2 - 2 * x3) * (x2 - 2 * x3) * (x2 - 2 * x3) + 10 * (x1 - x4) * (x1 - x4) * (x1 - x4) * (x1 - x4);
        };

        generateTables(MethodEnum.DAVIDON, f2, Vector.of(10, 0));
        generateTables(MethodEnum.DAVIDON, f2, Vector.of(15, 15));
        generateTables(MethodEnum.DAVIDON, f3, Vector.of(0, 0));
        generateTables(MethodEnum.DAVIDON, f3, Vector.of(10, 0));
        generateTables(MethodEnum.DAVIDON, f3, Vector.of(15, 15));


        generateTables(MethodEnum.POWELL, f1, Vector.of(0, 0));
        generateTables(MethodEnum.POWELL, f1, Vector.of(10, 0));
        generateTables(MethodEnum.POWELL, f1, Vector.of(15, 15));
        generateTables(MethodEnum.POWELL, f2, Vector.of(0, 0));
        generateTables(MethodEnum.POWELL, f2, Vector.of(10, 0));
        generateTables(MethodEnum.POWELL, f2, Vector.of(15, 15));
        generateTables(MethodEnum.POWELL, f3, Vector.of(0, 0));
        generateTables(MethodEnum.POWELL, f3, Vector.of(10, 0));
        generateTables(MethodEnum.POWELL, f3, Vector.of(15, 15));
    }

    private static void generateTables(final MethodEnum method, final Function<Vector, Double> function, final Vector startPoint) {
        System.out.println(method.name);
        final AbstractNewtonMethod optimizationMethod = switch (method) {
            case CLASSIC_NEWTON -> new ClassicNewtonMethod(function, 1e-5, startPoint);
            case ONE_DIRECTION_NEWTON -> new OneDirectionNewtonMethod(function, 1e-5, startPoint);
            case DESCEND_NEWTON -> new DescendMethod(function, 1e-5, startPoint);
            case DAVIDON -> new DavidonFletcherPowellMethod(function, 1e-5, startPoint);
            case POWELL -> new PowellMethod(function, 1e-5, startPoint);
        };
        try {
            optimizationMethod.minimize();
            String points = optimizationMethod.getSteps().toWolfram(function);
            System.out.printf("Epilog -> {PointSize[Medium], White, Line[%s], Point[%s]}]%n", points, points);
        } catch (final IllegalArgumentException e) {
            System.out.println(e.getMessage());
        }
    }

    private static void testAllMethods(final Function<Vector, Double> function, final Vector startPoint) {
        Arrays.stream(MethodEnum.values()).forEach(method -> generateTables(method, function, startPoint));
    }

    private static void onePointTwo() {
        final Function<Vector, Double> first = v -> {
            double x1 = v.get(0);
            double x2 = v.get(1);
            return x1 * x1 + x2 * x2 - 1.2 * x1 * x2;
        };
        final Vector firstStartPoint = Vector.of(4.0, 1.0);

        final Function<Vector, Double> second = v -> {
            double x1 = v.get(0);
            double x2 = v.get(1);
            return 100.0 * (x2 - x1 * x1) * (x2 - x1 * x1) + (1 - x1) * (1 - x1);
        };
        final Vector secondStartPoint = Vector.of(-1.2, 1.0);


        testAllMethods(first, firstStartPoint);
        testAllMethods(second, secondStartPoint);
    }
}
