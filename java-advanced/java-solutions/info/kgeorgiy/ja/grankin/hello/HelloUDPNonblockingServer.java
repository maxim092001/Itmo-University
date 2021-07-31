package info.kgeorgiy.ja.grankin.hello;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.ClosedSelectorException;
import java.nio.channels.DatagramChannel;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.charset.StandardCharsets;
import java.util.Iterator;
import java.util.Objects;
import java.util.Queue;
import java.util.concurrent.ConcurrentLinkedDeque;
import java.util.concurrent.Executors;

public class HelloUDPNonblockingServer extends AbstractServer {

    private Selector selector;
    private DatagramChannel channel;
    private Queue<MetaInfo> responses;
    private int bufferSize;

    public static void main(final String... args) {
        new HelloUDPNonblockingServer().parseAndStart(args);
    }

    @Override
    public void start(final int port, final int threads) {
        try {
            selector = Selector.open();
            channel = DatagramChannel.open();
            channel.configureBlocking(false);
            bufferSize = channel.socket().getReceiveBufferSize();
            channel.register(selector, SelectionKey.OP_READ);
            channel.bind(new InetSocketAddress(port));
            pool = Executors.newFixedThreadPool(threads);
            responses = new ConcurrentLinkedDeque<>();
            Executors.newSingleThreadExecutor().submit(this::startServer);
        } catch (final IOException e) {
            System.err.printf("Server setup error: %s%n On port: %d%n", e.getLocalizedMessage(), port);
        }
    }

    @Override
    public void close() {
        try {
            if (Objects.nonNull(channel)) channel.close();
            if (Objects.nonNull(selector)) selector.close();
            UDPUtils.terminationWait(pool, THREAD_TERMINATION_SECONDS);
        } catch (final IOException e) {
            System.err.printf("Resources close exception: %s%n", e.getLocalizedMessage());
        }
    }

    public void startServer() {
        while (!Thread.interrupted() && !channel.socket().isClosed()) {
            try {
                if (selector.select() > 0) {
                    for (final Iterator<SelectionKey> iter = selector.selectedKeys().iterator(); iter.hasNext(); ) {
                        final var key = iter.next();
                        try {
                            if (key.isValid()) {
                                if (key.isReadable()) {
                                    read(key);
                                } else {
                                    write(key);
                                }
                            }
                        } finally {
                            iter.remove();
                        }
                    }
                }
            } catch (final IOException e) {
                System.err.printf("Selector I/O exception: %s%n", e.getLocalizedMessage());
                close();
            } catch (final ClosedSelectorException e) {
                System.err.printf("Selector closed: %s%n", e.getLocalizedMessage());
            }
        }
    }

    private void write(final SelectionKey key) {
        if (!responses.isEmpty()) {
            final var metaInfo = responses.poll();
            final var buffer =
                    ByteBuffer.wrap(metaInfo.response.getBytes(StandardCharsets.UTF_8));
            try {
                channel.send(buffer, metaInfo.socket);
            } catch (final IOException e) {
                System.err.printf("Write I/O exception: %s", e.getLocalizedMessage());
            }
            key.interestOpsOr(SelectionKey.OP_READ);
        } else {
            key.interestOps(SelectionKey.OP_READ);
        }
    }

    private void read(final SelectionKey key) {
        try {
            final var buffer = ByteBuffer.allocate(bufferSize);
            final var address = channel.receive(buffer);
            pool.submit(() -> {
                buffer.flip();
                final var receive = StandardCharsets.UTF_8.decode(buffer).toString();
                final var response = helloResponse(receive);
                responses.add(MetaInfo.of(address, response));
                key.interestOps(SelectionKey.OP_WRITE);
                selector.wakeup();
            });
        } catch (final IOException e) {
            System.err.printf("Read I/O exception: %s", e.getLocalizedMessage());
        }
    }

    private static class MetaInfo {
        private final SocketAddress socket;
        private final String response;

        private MetaInfo(final SocketAddress socket, final String response) {
            this.socket = socket;
            this.response = response;
        }

        public static MetaInfo of(final SocketAddress address, final String response) {
            return new MetaInfo(address, response);
        }
    }
}
