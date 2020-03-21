package base;

import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class TestCounter {
    private final long start = System.currentTimeMillis();
    private int total = 0;
    private int passed = 0;
    public static final String JAR_EXT = ".jar";
    public static final SimpleDateFormat DATE_FORMAT = new SimpleDateFormat("dd.MM.yyyy HH:mm:ss");

    public void nextTest() {
        total++;
    }

    public int getTest() {
        return total;
    }

    public void passed() {
        passed++;
    }

    public void printStatus(final Class<?> clazz, final String... messages) {
        System.out.flush();
        System.err.println("===========================================");
        System.err.format("Test run: %d, passed: %d, failed: %d%n", total, passed, total - passed);
        System.err.format("Finished in %d ms%n", System.currentTimeMillis() - start);
        if (total != passed) {
            System.err.println("TESTS FAILED");
            System.exit(1);
        }
        System.err.println("Version: " + clazz.getSimpleName() + ", " + getVersion(clazz));
        for (final String message : messages) {
            System.err.println(message);
        }
    }


    public static String getVersion(final Class clazz) {
        try {
            final ClassLoader cl = clazz.getClassLoader();
            final URL url = cl.getResource(clazz.getName().replace('.', '/') + ".class");
            if (url == null) {
                return "(no manifest)";
            }

            final String path = url.getPath();
            final int index = path.indexOf(JAR_EXT);
            if (index == -1) {
                return DATE_FORMAT.format(new Date(new File(path).lastModified()));
            }

            final String jarPath = path.substring(0, index + JAR_EXT.length());
            try (JarFile jarFile = new JarFile(new File(new URI(jarPath)))) {
                final JarEntry entry = jarFile.getJarEntry("META-INF/MANIFEST.MF");
                return DATE_FORMAT.format(new Date(entry.getTime()));
            }
        } catch (IOException | URISyntaxException e) {
            return "error: " + e.toString();
        }
    }
}
