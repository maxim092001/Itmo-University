package lab3;

public class Utils {
    public static Vector generateRealX(final int n) {
        final double[] tmp = new double[n];
        for (int i = 0; i < n; i++) {
            tmp[i] = i + 1;
        }
        return new Vector(n, tmp);
    }

    public static double error(final int n, final int k) {
        final Vector realX = generateRealX(n);

        ProfileMatrix matrix = ProfileMatrixGenerator.generateDenseMatrix(n, k);
        Vector f = matrix.multiply(realX);

        matrix.computeLUDecomposition();
        Vector x = matrix.solve(f);
        x.sub(realX);
        return x.norm();
    }

    public static double diff(final int n, final int k) {
        return error(n, k) / generateRealX(n).norm();
    }
}
