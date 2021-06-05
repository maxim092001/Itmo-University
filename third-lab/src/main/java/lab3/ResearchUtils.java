package lab3;

import lab3.matrix.FullMatrix;
import lab3.matrix.ProfileMatrix;

public class ResearchUtils {
    private static final double INF = 1e9;

    public static Vector generateRealX(final int n) {
        final double[] tmp = new double[n];
        for (int i = 0; i < n; i++) {
            tmp[i] = i + 1;
        }
        return Vector.of(tmp);
    }

    public static double error(final int n, final int k, final Mode mode, final boolean isGauss) {
        final Vector realX = generateRealX(n);

        var matrix = switch (mode) {
            case DENSE -> ProfileMatrixGenerator.generateDenseMatrix(n, k);
            case GILBERT -> ProfileMatrixGenerator.generateGilbertMatrix(n);
        };

        final var profileMatrix = ProfileMatrix.of(matrix);
        final Vector f = profileMatrix.mul(realX);
//        if (isGauss) {
//            final var fullMatrix = new FullMatrix(matrix);
//            return fullMatrix.gauss(f, 1e-20).map(v -> v.sub(realX).norm()).orElse(INF);
//        } else {
//            profileMatrix.computeLUDecomposition();
//            Vector x = profileMatrix.solve(f);
//            x = x.sub(realX);
//            return x.norm();
//        }
        return 0;
    }


    enum Mode {
        DENSE,
        GILBERT
    }

    public static double diff(final int n, final int k, final Mode mode, final boolean isGauss) {
        return error(n, k, mode, isGauss) / generateRealX(n).norm();
    }
}
