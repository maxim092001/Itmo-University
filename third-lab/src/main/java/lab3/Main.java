package lab3;

public class Main {
    public static void main(String[] args) {
//        ProfileMatrix m = ProfileMatrix.of(
//                3,
//                new double[]{2.0, 3.0, 6.0},
//                new double[]{4.0, 7.0, 8.0},
//                new int[]{1, 1, 2, 4},
//                new double[]{1.0, 10.0, 42.0}
//        );
//
//        m.computeLUDecomposition();
//        for (int i = 0; i < m.n; i++) {
//            for (int j = 0; j < m.n; j++) {
//                System.out.print(m.get(i, j) + " ");
//            }
//            System.out.println();
//        }
//        System.out.println();
//
//        Vector solution = m.solve(new Vector(new double[]{1.0, 1.0, 1.0}, 3));
//        for (int i = 0; i < solution.size(); i++) {
//            System.out.print(solution.get(i) + " ");
//        }
//        System.out.println();


        for (int n = 1; n <= 30; n += 2) {
                var sub = Utils.error(n, 1, Utils.Mode.GILBERT, true);
                var div = Utils.diff(n, 1, Utils.Mode.GILBERT, true);
                System.out.printf("%d & %.15f & %.15f \\\\ %n \\hline %n", n, sub, div);
        }
//        var kek = ProfileMatrixGenerator.generateDenseMatrix(5, 1);
//        System.out.println(kek);
//        System.out.println(kek.solve(new Vector(5, 1, 2, 3, 4, 5)));

//        FullMatrix matrix = new FullMatrix(new double[][]{{1, 2, 3}, {4, 10, 6}, {7, 8, 42}});
//        Vector solution = matrix.gauss(new Vector(3, 1.0, 1.0, 1.0), 1e-5);
//        ProfileMatrix profileMatrix = matrix.toProfileMatrix();
//        System.out.println(solution);
    }
}
