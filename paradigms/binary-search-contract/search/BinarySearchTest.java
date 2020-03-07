package search;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class BinarySearchTest extends BinarySearchBaseTest {
    public static void main(final String... args) {
        test("BinarySearch", (x, a) -> {
            for (int i = 0; i < a.length; i++) {
                if (a[i] <= x) {
                    return longs(i);
                }
            }
            return longs(a.length);
        });
    }
}
