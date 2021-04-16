package lab2.gradient.methods;

import lab2.gradient.utils.DiagMatrix;
import lab2.gradient.utils.QuadraticFunction;
import lab2.gradient.utils.Vector;

import java.util.ArrayList;
import java.util.List;

public class ConjugateGradientMinimizer {
    private final QuadraticFunction f;
    private final List<Vector> x = new ArrayList<>();
    private final List<Vector> p = new ArrayList<>();
    private final List<Double> alpha = new ArrayList<>();
    private final List<Double> beta = new ArrayList<>();
    private final List<Vector> gradients = new ArrayList<>();
    private final double eps;

    public List<Vector> getX() {
        return x;
    }

    public ConjugateGradientMinimizer(QuadraticFunction f, Vector startPoint, double eps) {
        this.f = f;
        this.x.add(startPoint);

        Vector gradient0 = f.gradient(startPoint);
        this.gradients.add(gradient0);

        this.p.add(gradient0.mul(-1));

        this.eps = eps;
    }

    private void iteration(int k) {
        Vector pk = p.get(k);
        Vector aPk = (Vector) f.getA().mul(pk);
        double alphaK = gradients.get(k).sqrRate() / aPk.scalarMul(pk);
        alpha.add(alphaK);

        Vector xk = x.get(k);
        Vector xk1 = xk.add(pk.mul(alphaK));
        x.add(xk1);

        Vector gradientK = gradients.get(k);
        Vector gradientK1 = gradientK.add(aPk.mul(alphaK));
        gradients.add(gradientK1);

        double betaK = gradientK1.sqrRate() / gradientK.sqrRate();
        beta.add(betaK);

        Vector pk1 = gradientK1.mul(-1).add(pk.mul(betaK));
        p.add(pk1);
    }

    public void minimize() {
        double epsSqr = eps * eps;
        for (int i = 0; i <= f.dimensions(); i++) {
            iteration(i);
            if (gradients.get(gradients.size() - 1).sqrRate() < epsSqr) {
                break;
            }
        }
    }

    public Vector getMinX() {
        return x.get(x.size() - 1);
    }

    public double getMinF() {
        return f.apply(getMinX());
    }

    public static void main(String[] args) {
        QuadraticFunction f1 = QuadraticFunction.from2d(64, 126, 64, -10, 30, 13);
        ConjugateGradientMinimizer minimizer = new ConjugateGradientMinimizer(f1, new Vector(10.0, 15.0), 1e-5);
        minimizer.minimize();

        System.out.println("X = " + minimizer.getMinX());
        System.out.println("FX = " + minimizer.getMinF());
        System.out.println("number of iterations = " + minimizer.getX().size());
        System.out.println("X");
        for (Vector step : minimizer.getX()) {
            System.out.print(step.get(0) + ", ");
        }

        System.out.println();

        System.out.println("Y");
        for (Vector step : minimizer.getX()) {
            System.out.print(step.get(1) + ", ");
        }
    }
}