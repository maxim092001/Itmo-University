package lab3;

import lab3.matrix.ProfileMatrix;

import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import java.util.Scanner;


/**
 * Profile matrix generator.
 */
public class ProfileMatrixGenerator {

    /**
     * Generates matrix in profile format.
     *
     * @return profile format matrix.
     */
    public static ProfileMatrix generateMatrix() {
        var rnd = new Random(System.currentTimeMillis());
        int n = Math.abs(rnd.nextInt()) % 10;
        double[] di = new double[n];

        double[] r = new double[n];

        for (int i = 0; i < n; i++) {
            di[i] = rnd.nextDouble();
            r[i] = rnd.nextDouble();
        }

        int[] ia = new int[n + 1];
        ia[0] = ia[1] = 1;

        final List<Double> au = new ArrayList<>();
        final List<Double> al = new ArrayList<>();

        for (int i = 2; i <= n; i++) {
            int idx = Math.abs(rnd.nextInt()) % i;
            int t = i - idx - 1;
            ia[i] = ia[i - 1] + t;
            for (int j = 0; j < t; j++) {
                au.add((rnd.nextInt() % 2 == 0) ?  rnd.nextDouble() * (rnd.nextInt() % 100) : 0);
                al.add((rnd.nextInt() % 2 == 0) ?  rnd.nextDouble() * (rnd.nextInt() % 100) : 0);
            }
        }

        return ProfileMatrix.of(
                n,
                au.stream().mapToDouble(i -> i).toArray(),
                al.stream().mapToDouble(i -> i).toArray(),
                ia,
                di,
                r
        );
    }

    /**
     * Generates dense matrix dimension n.
     * @param n given n.
     * @param k given k for computing.
     * @return dense matrix.
     */
    public static double[][] generateDenseMatrix(int n, int k) {
        double[][] matrix = new double[n][n];
        Random random = new Random();
        for (int i = 0; i < n; i++) {
            for (int j = i; j < n; j++) {
                if (i != j) {
                    matrix[j][i] = -random.nextInt(5);
                    matrix[i][j] = -random.nextInt(5);
                }
            }
        }

        for (int i = 0; i < n; i++) {
            double sum = 0;
            for (int j = 0; j < n; j++) {
                if (i != j) {
                    sum += matrix[i][j];
                }
            }
            matrix[i][i] = -sum;
        }

        matrix[0][0] += Math.pow(10.0, -k);

        return matrix;
    }

    /**
     * Generates Gilbert matrix dimension n.
     * @param n given dimension.
     * @return Gilbert matrix.
     */
    public static double[][] generateGilbertMatrix(final int n) {
        double[][] matrix = new double[n][n];
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                matrix[i][j] = 1.0 / ((i + 1) + (j + 1) - 1);
            }
        }

        return matrix;
    }

    /**
     * Main method. Generates matrix and writes it to file.
     * @param args program arguments
     * @throws IOException if some exception with files occuired.
     */
    public static void main(String[] args) throws IOException {
        System.out.print("Enter directory name: ");
        final var sc = new Scanner(System.in);
        final String dirName = sc.next();
        Files.createDirectories(Path.of("tests"));
        final Path dirPath = Path.of("tests", dirName);
        final Path filePath = dirPath.resolve(Path.of("test.txt"));
        Files.createDirectories(dirPath);
        final FileWriter fl = new FileWriter(filePath.toString());
        fl.write(ProfileMatrixGenerator.generateMatrix().toString());
        fl.close();
    }
}
