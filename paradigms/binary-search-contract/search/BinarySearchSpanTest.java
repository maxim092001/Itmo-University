package search;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class BinarySearchSpanTest extends BinarySearchBaseTest {
    public static void main(final String... args) {
        test("BinarySearchSpan", (x, a) -> {
            for (int i = 0; i < a.length; i++) {
                if (a[i] == x) {
                    int j = i;
                    while (j < a.length && a[j] == x) {
                        j++;
                    }
                    return longs(i, j - i);
                }
                if (x > a[i]) {
                    return longs(i, 0);
                }
            }
            return longs(a.length, 0);
        });
    }
}
