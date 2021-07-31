package info.kgeorgiy.ja.grankin.implementor;

import info.kgeorgiy.java.advanced.implementor.Impler;
import info.kgeorgiy.java.advanced.implementor.ImplerException;
import info.kgeorgiy.java.advanced.implementor.JarImpler;

import javax.tools.JavaCompiler;
import javax.tools.ToolProvider;
import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.lang.reflect.Modifier;
import java.net.URISyntaxException;
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.jar.Attributes;
import java.util.jar.JarOutputStream;
import java.util.jar.Manifest;
import java.util.stream.Collectors;
import java.util.zip.ZipEntry;

/**
 * Class implementor.
 */
public class Implementor implements Impler, JarImpler {

    /**
     * Produces code implementing class or interface specified by provided {@code token}.
     * <p>
     * Token has to be valid {@link Implementor#isTokenValid(Class)}.
     *
     * @param token type token to create implementation for.
     * @param root  root directory.
     * @throws ImplerException if token not supported or I/O for output file.
     */
    @Override
    public void implement(final Class<?> token, final Path root) throws ImplerException {
        if (isTokenValid(token)) {
            final Path path = rootParents(token, root);

            try (BufferedWriter bufferedWriter = Files.newBufferedWriter(path)) {
                bufferedWriter.write(unicodeRepresentation(CodeGenerator.generate(token)));
            } catch (IOException e) {
                throw new ImplerException(String.format("Output file exception: %s", e.getLocalizedMessage()));
            }

        } else {
            throw new ImplerException(String.format("Token: %s not supported.", token.getCanonicalName()));
        }
    }

    /**
     * Converts string to unicode representation.
     *
     * @param s given string.
     * @return unicode representation.
     */
    private String unicodeRepresentation(final String s) {
        return s.chars().mapToObj(c -> String.format("\\u%04X", c)).collect(Collectors.joining());
    }

    /**
     * Creates new path containing parents for given root and token.
     *
     * @param token given token.
     * @param root  given root.
     * @return path with root parents.
     * @throws ImplerException if it failed to create parent directories.
     */
    private Path rootParents(final Class<?> token, final Path root) throws ImplerException {

        final Path parentsPath = root
                .resolve(token.getPackageName().replace('.', File.separatorChar))
                .resolve(String.format("%s%s.java", token.getSimpleName(), CodeGenerator.GeneratorUtils.IMPL));

        if (Objects.nonNull(parentsPath.getParent())) {
            try {
                Files.createDirectories(parentsPath.getParent());
            } catch (IOException e) {
                throw new ImplerException(
                        String.format("Exception while creating parent directories: %s", e.getLocalizedMessage())
                );
            }
        }

        return parentsPath;
    }

    /**
     * Returns {@code true} if token is valid and {@code false} if not.
     * <p>
     * Token valid if not:
     *
     * <ul>
     * <li>Primitive</li>
     * <li>Array</li>
     * <li>Enum</li>
     * <li>Final modifier</li>
     * <li>Private modifier</li>
     * </ul>
     *
     * @param token given token.
     * @return {@code true} if token is valid and {@code false} if not.
     */
    protected boolean isTokenValid(final Class<?> token) {
        final int modifiers = token.getModifiers();

        return !(token.isArray() ||
                token.isPrimitive() ||
                token == Enum.class ||
                Modifier.isFinal(modifiers) ||
                Modifier.isPrivate(modifiers)
        );
    }


    /**
     * Checking all arguments to be not null.
     *
     * @param args given arguments.
     * @return {@code true} if all arguments not null and {@code false} if not.
     */
    protected static boolean checkArgs(final String[] args) {
        if (Objects.isNull(args)) {
            System.err.println("No arguments");
            return false;
        }

        if (Arrays.stream(args).anyMatch(Objects::isNull)) {
            System.err.println("All args must be not null");
            return false;
        }

        return true;
    }

    /**
     * Compiles generated class by given token via System Compiler. {@link ToolProvider#getSystemJavaCompiler()}
     *
     * @param token given token.
     * @param path given path.
     * @throws ImplerException No java compiler provided/URI Exception/Compilation code different from 0
     */
    private void compileClassByTokenAndPath(final Class<?> token, final Path path) throws ImplerException {
        final JavaCompiler javaCompiler = ToolProvider.getSystemJavaCompiler();
        if (Objects.nonNull(javaCompiler)) {
            try {
                final Path classPath = Path.of(token.getProtectionDomain().getCodeSource().getLocation().toURI());
                final Path filePath = path.resolve(
                        Path.of(
                                token.getPackageName().replace('.', CodeGenerator.GeneratorUtils.CHAR_SEPARATOR),
                                String.format("%s%s.java", token.getSimpleName(), CodeGenerator.GeneratorUtils.IMPL)
                        )
                );
                final var args = List.of(
                        filePath.toString(),
                        "-cp",
                        String.format("%s%s%s", path, CodeGenerator.GeneratorUtils.PATH_SEPARATOR, classPath));
                final var code = javaCompiler.run(null, null, null, args.toArray(new String[0]));
                if (code != 0) {
                    throw new ImplerException("Compilation problems");
                }
            } catch (URISyntaxException e) {
                throw new ImplerException(String.format("URI Exception: %s", e.getLocalizedMessage()));
            }
        } else {
            throw new ImplerException("No java compiler");
        }
    }

    /**
     * Generates manifest by given token, temporary path and jar path.
     *
     * @param token given token.
     * @param path temporary path.
     * @param jarPath jar path.
     * @throws ImplerException if exception occurred during writing via {@link JarOutputStream}
     */
    private void generateManifest(final Class<?> token, final Path path, final Path jarPath) throws ImplerException {
        final var manifest = new Manifest();
        final var mainAttributes = manifest.getMainAttributes();

        mainAttributes.put(Attributes.Name.MANIFEST_VERSION, "1.0");

        try (final JarOutputStream jarOutputStream = new JarOutputStream(Files.newOutputStream(jarPath), manifest)) {
            final String className = String.format(
                    "%s/%s.class",
                    token.getPackageName().replace('.', '/'),
                    CodeGenerator.GeneratorUtils.generateClassImplByToken(token)
            );
            jarOutputStream.putNextEntry(new ZipEntry(className));
            Files.copy(Paths.get(path.toString(), className), jarOutputStream);
        } catch (IOException e) {
            throw new ImplerException(String.format("Exception while writing to JarOutput %s", e.getLocalizedMessage()));
        }
    }

    /**
     * Custom {@link FileVisitor} for directory deletions.
     */
    private static class DeleteFileVisitor extends SimpleFileVisitor<Path> {

        /**
         * Deletes visited files and continues.
         *
         * @param file {@link Path} to visit.
         * @param attrs {@link BasicFileAttributes} {@code file} attributes.
         * @return {@link FileVisitResult#CONTINUE}.
         * @throws IOException if deletion failed.
         */
        @Override
        public FileVisitResult visitFile(final Path file, final BasicFileAttributes attrs) throws IOException {
            Files.delete(file);
            return FileVisitResult.CONTINUE;
        }

        /**
         * Deletes directory after visiting all files and continues.
         *
         * @param dir to visit.
         * @param exc exception occurred during directory visiting.
         * @return {@link FileVisitResult#CONTINUE}.
         * @throws IOException if deletion failed.
         */
        @Override
        public FileVisitResult postVisitDirectory(final Path dir, final IOException exc) throws IOException {
            Files.delete(dir);
            return FileVisitResult.CONTINUE;
        }
    }

    /**
     * Produces jar containing code implementation generated via {@link Implementor#implement(Class, Path)}
     *
     * Each token has to be valid {@link Implementor#isTokenValid(Class)}.
     *
     * @param token type token to create implementation for.
     * @param jarFile target <var>.jar</var> file.
     * @throws ImplerException if token is not supported or I/O exception occurred.
     */
    @Override
    public void implementJar(final Class<?> token, final Path jarFile) throws ImplerException {
        if (isTokenValid(token)) {
            if (Objects.nonNull(jarFile.getParent())) {
                final Path tempPath;
                try {
                    Files.createDirectories(jarFile.getParent());
                    tempPath = Files.createTempDirectory(jarFile.toAbsolutePath().getParent(), "temp");
                    try {
                        implement(token, tempPath);
                        compileClassByTokenAndPath(token, tempPath);
                        generateManifest(token, tempPath, jarFile);
                    } finally {
                        Files.walkFileTree(tempPath, new DeleteFileVisitor());
                    }
                } catch (IOException e) {
                    throw new ImplerException(String.format("I/O exception for jarFile: %s", jarFile.toString()));
                }
            }
        } else {
            throw new ImplerException(String.format("Token: %s not supported.", token.getCanonicalName()));
        }
    }

    /**
     * Point of entry to {@link Implementor}.
     *
     * <p>There should be two or three arguments. All arguments have to be defined (not null).
     * For three arguments and first argument equals {@code -jar} runs {@link JarImpler#implementJar(Class, Path)}.
     *
     * @param args given arguments.
     */
    public static void main(String[] args) {
        if (!checkArgs(args)) {
            return;
        }

        try {
            var implementor = new Implementor();
            if (args.length == 2) {
                implementor.implement(Class.forName(args[0]), Path.of(args[1]));
            } else if (args.length == 3 && args[0].equals("-jar")) {
                implementor.implementJar(Class.forName(args[1]), Path.of(args[2]));
            } else {
                System.err.println("Incorrect command for implementor");
            }
        } catch (InvalidPathException e) {
            System.err.printf("Incorrect path: %s\n", e.getLocalizedMessage());
        } catch (ClassNotFoundException e) {
            System.err.printf("Incorrect class name: %s\n", e.getLocalizedMessage());
        } catch (ImplerException e) {
            System.err.printf("Implementor exception: %s", e.getLocalizedMessage());
        }
    }

}
