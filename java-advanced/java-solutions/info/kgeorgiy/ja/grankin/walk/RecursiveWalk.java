package info.kgeorgiy.ja.grankin.walk;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.*;
import java.util.Map;

public class RecursiveWalk {

    private static final Map<Class<? extends Exception>, String> EXCEPTION_MESSAGE = Map.of(
            AccessDeniedException.class, "Access denied: ",
            FileNotFoundException.class, "File not found: ",
            FileSystemException.class, "File system error: ",
            SecurityException.class, "Security exception error: ",
            InvalidPathException.class, "Invalid path: ",
            NoSuchFileException.class, "No such file: ",
            FileSystemNotFoundException.class, "File system not found: "
    );


    public static void main(String[] args) {
        if (args == null || args.length != 2 || args[0] == null || args[1] == null) {
            System.err.println("Expected two arguments");
        } else {
            walkThroughFiles(args);
        }
    }

    private static void walkThroughFiles(final String... paths) {
        try {
            Path inputPath = Paths.get(paths[0]);
            Path outputPath = Paths.get(paths[1]);
            try (BufferedReader bufferedReader = Files.newBufferedReader(inputPath)) {
                try(BufferedWriter bufferedWriter = Files.newBufferedWriter(outputPath)) {
                    if (outputPath.getParent() != null) {
                        Files.createDirectories(outputPath.getParent());
                    }
                    PJWHashFileVisitor pjwHashFileVisitor = new PJWHashFileVisitor(bufferedWriter);
                    String filePath;
                    while ((filePath = bufferedReader.readLine()) != null) {
                        try {
                            Path currentPath = Paths.get(filePath);
                            Files.walkFileTree(currentPath, pjwHashFileVisitor);
                            // :NOTE: ignored message
                        } catch (InvalidPathException e) {
                            pjwHashFileVisitor.writeHashAndPathFormatted(bufferedWriter, 0, filePath);
                        }

                    }
                } catch (IOException e) {
                    System.err.format("Output file: %s \nException: %s\n", outputPath.toString(), e.getLocalizedMessage());
                }
            } catch (IOException e) {
                System.err.format("Input file: %s \nException: %s\n", inputPath.toString(), e.getLocalizedMessage());
            }
        } catch (SecurityException | InvalidPathException | FileSystemNotFoundException e) {
            var exceptionClass = e.getClass();
            System.err.format("%s %s\n",
                    EXCEPTION_MESSAGE.getOrDefault(exceptionClass, "Error: " + exceptionClass.getName()),
                    e.getLocalizedMessage()
            );
        }
    }
}
