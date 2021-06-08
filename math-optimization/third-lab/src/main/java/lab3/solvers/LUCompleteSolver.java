package lab3.solvers;

import lab3.Vector;
import lab3.matrix.LUMatrix;
import lab3.matrix.LUView;
import lab3.matrix.ProfileMatrix;

public class LUCompleteSolver implements Solver<ProfileMatrix> {
    private static final LUSolver luSolver = new LUSolver();

    private LUView luView;

    @Override
    public Vector solve(ProfileMatrix a, Vector b) {
        a = a.copy();
        luView = a.luDecomposition();
        return luSolver.solve(new LUMatrix(getLUView()), b);
    }

    public LUView getLUView() {
        return luView;
    }
}
