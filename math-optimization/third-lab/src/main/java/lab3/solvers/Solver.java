package lab3.solvers;

import lab3.Vector;
import lab3.matrix.MatrixView;

public interface Solver<T extends MatrixView> {
    Vector solve(T a, Vector b);
}
