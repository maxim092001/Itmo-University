package base;

import java.util.List;
import java.util.Locale;
import java.util.Objects;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class Asserts {
    static {
        Locale.setDefault(Locale.US);
    }

    public static void assertEquals(final String message, final Object expected, final Object actual) {
        assertTrue(String.format("%s:%n     expected `%s`,%n       actual `%s`", message, expected, actual), Objects.equals(expected, actual));
    }

    public static void assertEquals(final String message, final List<String> expected, final List<String> actual) {
        for (int i = 0; i  < Math.min(expected.size(), actual.size()); i++) {
            assertEquals(message + ":" + (i + 1), expected.get(i), actual.get(i));
        }
        assertEquals(message + ": Number of items", expected.size(), actual.size());
    }

    public static void assertTrue(final String message, final boolean value) {
        if (!value) {
            throw error("%s", message);
        }
    }

    public static void assertEquals(final String message, final double expected, final double actual, final double precision) {
        final double error = Math.abs(expected - actual);
        assertTrue(
                String.format("%s: expected %f, found %f", message, expected, actual),
                error <= precision || (Math.abs(expected) >= 1 && error / Math.abs(expected) < precision)
        );
    }

    public static void assertSame(final String message, final Object expected, final Object actual) {
        assertTrue(String.format("%s: expected same objects: %s and %s", message, expected, actual), expected == actual);
    }

    protected static void checkAssert(final Class<?> c) {
        if (!c.desiredAssertionStatus()) {
            throw error("You should enable assertions by running 'java -ea %s'", c.getName());
        }
    }

    public static AssertionError error(final String format, final Object... args) {
        return new AssertionError(String.format(format, args));
    }
}
