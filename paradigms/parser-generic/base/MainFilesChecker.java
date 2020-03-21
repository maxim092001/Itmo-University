package base;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class MainFilesChecker extends MainChecker {
    public MainFilesChecker(final String className) {
        super(className);
    }

    private Path getFile(final String suffix) {
        return Paths.get(String.format("test%d.%s", counter.getTest() + 1, suffix));
    }

    protected List<String> runFiles(final List<String> input) {
        try {
            final Path inf = getFile("in");
            final Path ouf = getFile("out");
            Files.write(inf, input);
            run(inf.toString(), ouf.toString());
            final List<String> output = Files.readAllLines(ouf);
            Files.delete(inf);
            Files.delete(ouf);
            return output;
        } catch (final IOException e) {
            throw new AssertionError(e);
        }
    }
}
