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
    private long start = System.currentTimeMillis();
    private int total = 0;
    private int passed = 0;

    public void nextTest() {
        total++;
    }

    public int getTest() {
        return total;
    }

    public void passed() {
        passed++;
    }

    public void printStatus(final Class<?> aClass) {
        System.err.println("===========================================");
        System.err.format("Test run: %d, passed: %d, failed: %d%n", total, passed, total - passed);
        System.err.format("Finished in %d ms%n", System.currentTimeMillis() - start);
        if (total != passed) {
            System.err.println("TESTS FAILED");
            System.exit(1);
        }
        System.err.println("Version: " + getVersion(aClass));
    }


    public static String getVersion(final Class clazz) {
        try {
            ClassLoader cl = clazz.getClassLoader();
            URL url = cl.getResource(clazz.getName().replace('.', '/') + ".class");
            if (url == null) {
                return "(no manifest)";
            } else {
                String path = url.getPath();
                String jarExt = ".jar";
                int index = path.indexOf(jarExt);
                SimpleDateFormat sdf = new SimpleDateFormat("dd.MM.yyyy HH:mm:ss");
                if (index != -1) {
                    String jarPath = path.substring(0, index + jarExt.length());
                    File file = new File(jarPath);
                    String jarVersion = file.getName();
                    try (JarFile jarFile = new JarFile(new File(new URI(jarPath)))) {
                        JarEntry entry = jarFile.getJarEntry("META-INF/MANIFEST.MF");
                        return sdf.format(new Date(entry.getTime()));
                    }
                } else {
                    return sdf.format(new Date(new File(path).lastModified()));
                }
            }
        } catch (IOException | URISyntaxException e) {
            return "error: " + e.toString();
        }
    }
}
