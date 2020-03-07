package search;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class BinarySearchMissingTest extends BinarySearchBaseTest {
    public static void main(final String... args) {
        test("BinarySearchMissing", (x, a) -> {
            for (int i = 0; i < a.length; i++) {
                if (a[i] == x) {
                    return longs(i);
                }
                if (x > a[i]) {
                    return longs(-1 - i);
                }
            }
            return longs(-1 - a.length);
        });
    }
}
