package base;

import java.util.List;
import java.util.Random;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class Randomized {
    public static final String ENGLISH = "abcdefghijklmnopqrstuvwxyz";
    public static final String RUSSIAN = "абвгдеежзийклмнопрстуфхцчшщъыьэюя";
    public static final String GREEK = "αβγŋδεζηθικλμνξοπρτυφχψω";

    public final Random random = new Random(8045702385702345702L);

    public String randomString(final String chars) {
        return randomChar(chars) + (random.nextBoolean() ? "" : randomString(chars));
    }

    public char randomChar(final String chars) {
        return chars.charAt(random.nextInt(chars.length()));
    }

    public String randomString(final String chars, final int length) {
        final StringBuilder string = new StringBuilder();
        for (int i = 0; i < length; i++) {
            string.append(randomChar(chars));
        }
        return string.toString();
    }

    public String randomString(final String chars, final int minLength, int maxLength) {
        return randomString(chars, randomInt(minLength, maxLength + 1));
    }

    public int randomInt(final int min, final int max) {
        return random.nextInt(max - min) + min;
    }

    @SafeVarargs
    public final <T> T randomItem(final T... items) {
        return items[random.nextInt(items.length)];
    }

    public final <T> T randomItem(final List<T> items) {
        return items.get(random.nextInt(items.size()));
    }

    public Random getRandom() {
        return random;
    }
}
