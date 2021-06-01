package lab4;

import lab4.matrix.Vector;
import lab4.newton.ClassicNewtonMethod;
import lab4.newton.DescendMethod;
import lab4.newton.OneDirectionNewtonMethod;

import java.util.Arrays;

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
    }
}
