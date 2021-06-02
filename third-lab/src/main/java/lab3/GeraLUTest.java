package lab3;

import lab3.matrix.LUView;
import lab3.matrix.MatrixUtils;
import lab3.matrix.MultiplyMatrixView;
import lab3.matrix.ProfileMatrix;

public class GeraLUTest {
    private static void singleTest(double[][] a) {
        ProfileMatrix a1 = ProfileMatrix.of(a);
        ProfileMatrix a1s = new ProfileMatrix(a1);
        LUView luView = a1.computeLUDecomposition();
        MultiplyMatrixView multiplyMatrixView = new MultiplyMatrixView(luView.getL(), luView.getU());
        if (!MatrixUtils.equals(multiplyMatrixView, a1s)) {
            System.err.println(MatrixUtils.toString(multiplyMatrixView));
            System.err.println("L=");
            System.err.println(MatrixUtils.toString(luView.getL()));
            System.err.println("R=");
            System.err.println(MatrixUtils.toString(luView.getU()));
            throw new RuntimeException();
        }
    }

    public static void main(String[] args) {
        singleTest(new double[][]{
                {1.0, 2.0, 3.0},
                {4.0, 7.0, 11.0},
                {181.0, 7.0, 94.0}
        });
    }
}
