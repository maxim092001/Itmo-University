package lab3.matrix;

public class MatrixUtils {
    private MatrixUtils() {}

    public static boolean equals(MatrixView a, MatrixView b, double threshold) {
        if (a.getN() != b.getN()) {
            return false;
        }

        for (int i = 0; i < a.getN(); i++) {
            for (int j = 0; j < a.getN(); ++j) {
                if (Math.abs(a.get(i, j) - b.get(i, j)) > threshold) {
                    return false;
                }
            }
        }

        return true;
    }

    public static boolean equals(MatrixView a, MatrixView b) {
        return equals(a, b, 1e-9);
    }

    public static String toString(MatrixView a) {
        StringBuilder builder = new StringBuilder();
        for (int i = 0; i < a.getN(); i++) {
            builder.append('[').append(a.get(i, 0));
            for (int j = 1; j < a.getN(); j++) {
                builder.append(", ").append(a.get(i ,j));
            }
            builder.append("]\n");
        }
        return builder.toString();
    }
}
