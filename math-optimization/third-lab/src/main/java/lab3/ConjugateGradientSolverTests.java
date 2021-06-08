package lab3;

import lab3.matrix.MatrixUtils;
import lab3.matrix.SparseMatrix;
import lab3.solvers.ConjugateGradientSolver;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Random;

public class ConjugateGradientSolverTests {
    private static void simpleTest(double[][] a, double[] ans) {
        System.out.println("\\begin{equation*}");
        SparseMatrix A = new SparseMatrix(a);
        Vector Ans = Vector.of(ans);
        Vector b = A.mul(Ans);

        System.out.println("A=");
        System.out.println(MatrixUtils.toLatex(A));
        System.out.println("\\qquad\nx^*=");
        System.out.println(MatrixUtils.toLatex(Ans));
        System.out.println("\\qquad\nb=");
        System.out.println(MatrixUtils.toLatex(b));

        ConjugateGradientSolver solver = new ConjugateGradientSolver(Vector.zero(A.getN()));
        Vector x = solver.solve(A, b);
        System.out.println("\\qquad\nx");
        System.out.println(MatrixUtils.toLatex(x));

        System.out.println("\\end{equation*}");
//        System.out.println(x);
//        System.out.println(Ans);
    }

    private static final double[] AIJ = new double[]{0, -1, -2, -3, -4};

    private static SparseMatrix generate52(int n) {
        Random rnd = new Random(0xDEAD);

        double[][] a = new double[n][n];
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < i; j++) {
                a[i][j] = AIJ[rnd.nextInt(AIJ.length)];
                a[j][i] = a[i][j];
            }
        }

        for (int i = 0; i < n; i++) {
            double sum = 0;
            for (int j = 0; j < n; j++) {
                if (i != j) {
                    sum += a[i][j];
                }
            }

            if (i == 0) {
                a[i][i] = -sum;
            } else {
                a[i][i] = 1 - sum;
            }
        }

        return new SparseMatrix(a);
    }

    @Data
    @NoArgsConstructor
    private static class ExperimentResult {
        private int n;
        private int iterationCount;
        private double absError;
        private double error;
        private double cond;
    }

    private static ExperimentResult experiment(SparseMatrix a) {
        int n = a.getN();
        Vector ans = Vector.natural(n);
        Vector b = a.mul(ans);

        ConjugateGradientSolver solver = new ConjugateGradientSolver(Vector.zero(n));
        Vector x = solver.solve(a, b);

        ExperimentResult result = new ExperimentResult();
        result.setN(n);
        result.setIterationCount(solver.getIterationCount());

        double absError = ans.sub(x).norm();

        result.setAbsError(absError);

        double error = absError / ans.norm();

        result.setError(error);
        result.setCond(error / ((b.sub(a.mul(x))).norm() / b.norm()));
        return result;
    }

    private static void experiments52() {
        int[] dims = new int[]{2, 3, 4, 5, 6, 7, 8, 9, 10, 15, 20, 25, 50, 100, 250, 500, 1000, 2500, 5000, 10000, 25000};
        System.out.println("\\begin{longtable}{|c|c|c|c|c|}\n\\hline\n");
        for (int n : dims) {
            ExperimentResult result = experiment(generate52(n));
            System.out.print(result.getN());
            System.out.print(" & ");
            System.out.print(result.getIterationCount());
            System.out.print(" & ");
            System.out.printf("%g", result.getAbsError());
            System.out.print(" & ");
            System.out.printf("%g", result.getError());
            System.out.print(" & ");
            System.out.printf("%g", result.getCond());
            System.out.println(" \\\n\\hline");
        }
        System.out.println("\\end{longtable}");
    }

    public static void main(String[] args) {
        experiments52();

//        simpleTest(new double[][]{
//                {1, 0},
//                {0, 1}
//        }, new double[]{1, 1});
//
//        simpleTest(new double[][]{
//                {5, -2},
//                {-2, 1}
//        }, new double[]{0, 3});
//
//        simpleTest(new double[][]{
//                {5, -2, 14},
//                {-2, 1, 7},
//                {14, 7, -1},
//        }, new double[]{19, 84, 115});
//
//        simpleTest(new double[][]{
//                {18, -20, 148, 1337},
//                {-20, 1548, 77, 15},
//                {148, 77, 8412, -1},
//                {1337, 15, -1, 844}
//        }, new double[]{-87, -999, 265, 148});
    }
}
