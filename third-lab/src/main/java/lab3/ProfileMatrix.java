package lab3;

import java.util.Arrays;

public class ProfileMatrix {
    final int n;
    final double[] au;
    final double[] al;
    final double[] r;
    final int[] ia;
    final double[] di;

    private ProfileMatrix(
            final int n,
            final double[] au,
            final double[] al,
            final double[] r,
            final int[] ia,
            final double[] di
    ) {
        this.n = n;
        this.au = au;
        this.al = al;
        this.r = r;
        this.ia = ia;
        this.di = di;
    }

    public static ProfileMatrix of(
            final int n,
            final double[] au,
            final double[] al,
            final double[] r,
            final int[] ia,
            final double[] di
    ) {
        return new ProfileMatrix(n, au, al, r, ia, di);
    }

    @Override
    public String toString() {
        final String lineSeparator = System.lineSeparator();

        return "n=" + n + lineSeparator +
                "au=" + Arrays.toString(au) + lineSeparator +
                "al=" + Arrays.toString(al) + lineSeparator +
                "r=" + Arrays.toString(r) + lineSeparator +
                "ia=" + Arrays.toString(ia) + lineSeparator +
                "di=" + Arrays.toString(di) + lineSeparator;
    }
}
