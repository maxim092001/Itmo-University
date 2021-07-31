package info.kgeorgiy.ja.grankin.hello;

import info.kgeorgiy.java.advanced.hello.HelloServer;

import java.util.Arrays;
import java.util.Objects;
import java.util.concurrent.ExecutorService;

public abstract class AbstractServer implements HelloServer {
    protected ExecutorService pool;

    protected static final long THREAD_TERMINATION_SECONDS = Long.MAX_VALUE;

    protected void parseAndStart(final String... args) {
        if (Objects.isNull(args) || args.length != 2 || Arrays.stream(args).anyMatch(Objects::isNull)) {
            System.err.println("Expected 2 non null arguments");
        } else {
            try {
                final int port = Integer.parseInt(args[0]);
                final int threads = Integer.parseInt(args[1]);
                try (final HelloServer server = new HelloUDPServer()) {
                    server.start(port, threads);
                }
            } catch (final NumberFormatException e) {
                System.err.printf("Arguments have to be integer: %s%n", e.getLocalizedMessage());
            }
        }
    }

    protected String helloResponse(final String s) {
        return "Hello, ".concat(s);
    }
}
