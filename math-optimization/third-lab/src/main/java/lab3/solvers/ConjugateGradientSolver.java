package lab3.solvers;

import lab3.Vector;
import lab3.matrix.SparseMatrix;

import java.util.AbstractList;
import java.util.List;
import java.util.RandomAccess;

public class ConjugateGradientSolver implements Solver<SparseMatrix> {
    private static final int MAX_ITERATIONS = 10000;
    private static final double EPS = 1e-12;

    private final List<Vector> x = new ForgettingList<>();
    private final List<Vector> r = new ForgettingList<>();
    private final List<Double> rScalarSqr = new ForgettingList<>();
    private final List<Vector> z = new ForgettingList<>();
    private final double eps;

    public ConjugateGradientSolver(Vector x0) {
        this(x0, EPS);
    }

    public ConjugateGradientSolver(Vector x0, double eps) {
        this.x.add(x0);
        this.eps = eps;
    }

    private static void checkSymmetry(SparseMatrix a) {
        double[] al = a.getAl();
        double[] au = a.getAu();
        assert al.length == au.length;
        for (int i = 0; i < al.length; i++) {
            if (Math.abs(al[i] - au[i]) > EPS) {
                throw new IllegalArgumentException();
            }
        }
    }

    public int getIterationCount() {
        return x.size();
    }

    @Override
    public Vector solve(SparseMatrix a, Vector b) {
        checkSymmetry(a);
        r.add(b.sub(a.mul(x.get(0))));
        rScalarSqr.add(r.get(0).scalarMul(r.get(0)));
        z.add(r.get(0));

        for (int k = 1; k < MAX_ITERATIONS; k++) {
            Vector rK1 = r.get(k - 1);
            Vector zK1 = z.get(k - 1);
            double alpha = rScalarSqr.get(k - 1) / a.mul(zK1).scalarMul(zK1);

            x.add(x.get(k - 1).add(zK1.mul(alpha)));

            Vector rK = rK1.sub(a.mul(zK1).mul(alpha));

            if (rK.norm() / b.norm() < eps) {
                return x.get(k);
            }

            r.add(rK);
            rScalarSqr.add(rK.scalarMul(rK));

            double beta = rScalarSqr.get(k) / rScalarSqr.get(k - 1);
            Vector zK = rK.add(zK1.mul(beta));
            z.add(zK);
        }

        return null;
    }

    private static final class ForgettingList<T> extends AbstractList<T> implements RandomAccess {
        private int n = 0;
        private T a, b;

        @Override
        public T get(int index) {
            if (index < n - 2 || index >= n) {
                throw new IllegalArgumentException();
            } else if (index == n - 2) {
                return a;
            } else {
                return b;
            }
        }

        @Override
        public int size() {
            return n;
        }

        @Override
        public boolean add(T t) {
            n++;
            a = b;
            b = t;
            return true;
        }
    }
}
