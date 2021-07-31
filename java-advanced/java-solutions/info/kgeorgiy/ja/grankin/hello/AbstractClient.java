package info.kgeorgiy.ja.grankin.hello;

import info.kgeorgiy.java.advanced.hello.HelloClient;

import java.util.Arrays;
import java.util.Objects;

public abstract class AbstractClient implements HelloClient {

    protected static final long REQUEST_TIMEOUT = 5L;
    protected static final int SOCKET_TIMEOUT = 500;

    protected void parseArgsAndRunClient(final String... args) {
        if (Objects.isNull(args) || args.length != 5 || Arrays.stream(args).anyMatch(Objects::isNull)) {
            System.err.println("Expected 5 non null arguments");
        } else {
            try {
                final String host = args[0];
                final int port = Integer.parseInt(args[1]);
                final String prefix = args[2];
                final int threads = Integer.parseInt(args[3]);
                final int requests = Integer.parseInt(args[4]);
                run(host, port, prefix, threads, requests);
            } catch (final NumberFormatException e) {
                System.err.printf("Arguments expected to be integer: %s%n", e.getLocalizedMessage());
            }
        }
    }

    protected String joinRequest(final String prefix, final int thread, final int id) {
        return String.join("", prefix, Integer.toString(thread), "_", Integer.toString(id));
    }
}
