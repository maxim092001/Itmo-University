package lab2.gradient.methods;

import lab2.gradient.utils.DiagMatrix;
import lab2.gradient.utils.QuadraticFunction;
import lab2.gradient.utils.Vector;

import java.util.ArrayList;
import java.util.List;

/**
 * Conjugate gradient minimizer.
 */
public class ConjugateGradientMinimizer {

    /**
     * Given function.
     */
    private final QuadraticFunction f;

    /**
     * Iteration steps.
     */
    private final List<IterationStep> steps = new ArrayList<>();

    /**
     *
     */
    private final List<Vector> p = new ArrayList<>();

    /**
     * Alpha coefficient.
     */
    private final List<Double> alpha = new ArrayList<>();

    /**
     * Beta coefficient.
     */
    private final List<Double> beta = new ArrayList<>();

    /**
     * Gradients.
     */
    private final List<Vector> gradients = new ArrayList<>();

    /**
     * Given epsilon.
     */
    private final double eps;

    /**
     * Constructs conjugate gradient method.
     *
     * @param f given function.
     * @param startPoint given start point.
     * @param eps given epsilon.
     */
    public ConjugateGradientMinimizer(
            final QuadraticFunction f,
            final Vector startPoint,
            final double eps
    ) {
        this.f = f;
        this.steps.add(new IterationStep(0, startPoint, f.apply(startPoint)));

        final Vector gradient0 = f.gradient(startPoint);
        this.gradients.add(gradient0);

        this.p.add(gradient0.mul(-1));

        this.eps = eps;
    }

    /**
     * New iteration.
     *
     * @param k last iteration step.
     */
    private void iteration(final int k) {
        final Vector pk = p.get(k);
        final Vector aPk = (Vector) f.getA().mul(pk);
        final double alphaK = gradients.get(k).sqrRate() / aPk.scalarMul(pk);
        alpha.add(alphaK);

        final Vector xk = steps.get(k).getVector();
        final Vector xk1 = xk.add(pk.mul(alphaK));
        steps.add(new IterationStep(k + 1, xk1, f.apply(xk1))); // calculating function only for step logging.

        final Vector gradientK = gradients.get(k);
        final Vector gradientK1 = gradientK.add(aPk.mul(alphaK));
        gradients.add(gradientK1);

        final double betaK = gradientK1.sqrRate() / gradientK.sqrRate();
        beta.add(betaK);

        final Vector pk1 = gradientK1.mul(-1).add(pk.mul(betaK));
        p.add(pk1);
    }

    /**
     * Minimize given function.
     */
    public void minimize() {
        final double epsSqr = eps * eps;
        for (int i = 0; i <= f.dimensions(); i++) {
            iteration(i);
            if (gradients.get(gradients.size() - 1).sqrRate() < epsSqr) {
                break;
            }
        }
    }

    /**
     * Get minimum argument.
     *
     * @return minimum argument.
     */
    public Vector getMinX() {
        return steps.get(steps.size() - 1).getVector();
    }

    /**
     * Get function result in minimum argument.
     *
     * @return function result.
     */
    public double getMinF() {
        return f.apply(getMinX());
    }

    public static void main(String[] args) {
        QuadraticFunction f3 = QuadraticFunction.from2d(1, 2, 3, 4 , 5, 6);
        QuadraticFunction f4 = QuadraticFunction.from2d(254, 506, 254, 50, 130, -111);

        QuadraticFunction f2 = QuadraticFunction.from2d(1, 2, 1, 0, 0, 0);
        QuadraticFunction f1 = new QuadraticFunction(new DiagMatrix(1, 1), new Vector(0, 0), 0);
        System.out.println(f1);
        System.out.println(f1.apply(new Vector(5, -19)));
        System.out.println(f2);
        System.out.println(f2.apply(new Vector(5, -19)));

        System.out.println(f1);
        ConjugateGradientMinimizer minimizer = new ConjugateGradientMinimizer(f2, new Vector(5, 10), 1e-5);
        minimizer.minimize();
        System.out.println(minimizer.getMinX());
        System.out.println(minimizer.getMinF());
    }
}