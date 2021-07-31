package info.kgeorgiy.ja.grankin.walk;

import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;

public class PJWHashFileVisitor implements FileVisitor<Path> {

    private static final int BUFFER_SIZE = 1024;
    private static final long HASH_INITIAL_VALUE = 0L;
    private static final byte[] buffer = new byte[BUFFER_SIZE];
    private final BufferedWriter bufferedWriter;

    public PJWHashFileVisitor(final BufferedWriter bufferedWriter) {
        this.bufferedWriter = bufferedWriter;
    }

    @Override
    public FileVisitResult preVisitDirectory(Path dir, BasicFileAttributes attrs) {
        return FileVisitResult.CONTINUE;
    }

    @Override
    public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) throws IOException {
        calculateFileHash(file, bufferedWriter);
        return FileVisitResult.CONTINUE;
    }

    @Override
    public FileVisitResult visitFileFailed(Path file, IOException exc) throws IOException {
        writeHashAndPathFormatted(bufferedWriter, HASH_INITIAL_VALUE, file.toString());
        return FileVisitResult.CONTINUE;
    }

    @Override
    public FileVisitResult postVisitDirectory(Path dir, IOException exc) {
        return FileVisitResult.CONTINUE;
    }

    public final void calculateFileHash(final Path filePath, final BufferedWriter writer) throws IOException {
        long currentHash = HASH_INITIAL_VALUE;
        try (InputStream input = Files.newInputStream(filePath)) {
            int numberOfBytes;
            while ((numberOfBytes = input.read(buffer)) >= 0) {
                currentHash = hashPjw(currentHash, numberOfBytes);
            }
        } catch (InvalidPathException | IOException e) {
            currentHash = 0;
        } finally {
            writeHashAndPathFormatted(writer, currentHash, filePath.toString());
        }
    }

    public final void writeHashAndPathFormatted(final BufferedWriter writer, final long hash, final String path) throws IOException {
        // :NOTE: \n
        writer.write(String.format("%016x %s\n", hash, path));
    }

    private long hashPjw(final long prevHash, final int size) {
        final long bitesSize = 64L;
        final long highBits = 0xFFFF_FFFF_FFFF_FFFFL << (bitesSize / 8L * 7L);
        long hash = prevHash;
        for (int i = 0; i < size; i++) {
            hash = (hash << (bitesSize / 8L)) + (PJWHashFileVisitor.buffer[i] & 0xFF);
            final long hashAndHighBits;
            if ((hashAndHighBits = hash & highBits) != 0) {
                hash ^= hashAndHighBits >> (bitesSize / 4 * 3);
                hash &= ~hashAndHighBits;
            }
        }
        return hash;
    }
}
