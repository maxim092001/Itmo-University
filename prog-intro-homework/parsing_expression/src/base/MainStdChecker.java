package base;

import java.io.*;
import java.util.List;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class MainStdChecker extends MainChecker {

    public MainStdChecker(final String className) {
        super(className);
    }

    protected List<String> runStd(final List<String> input) {
        final ByteArrayOutputStream baos = new ByteArrayOutputStream();
        try (final PrintWriter writer = new PrintWriter(baos)) {
            input.forEach(writer::println);
        }

        final InputStream oldIn = System.in;
        try {
            System.setIn(new ByteArrayInputStream(baos.toByteArray()));
            return runComment(String.format("<%d input lines>", input.size()));
        } finally {
            System.setIn(oldIn);
        }
    }
}
