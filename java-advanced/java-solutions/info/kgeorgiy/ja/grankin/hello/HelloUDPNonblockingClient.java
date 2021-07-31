package info.kgeorgiy.ja.grankin.hello;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.DatagramChannel;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Iterator;

public class HelloUDPNonblockingClient extends AbstractClient {

    public static void main(final String... args) {
        new HelloUDPNonblockingClient().parseArgsAndRunClient(args);
    }

    @Override
    public void run(final String host, final int port, final String prefix, final int threads, final int requests) {

        class MetaInfo {
            final int threadId;
            final ByteBuffer byteBuffer;
            int requestId;

            public MetaInfo(final int threadId, final int bufferSize) {
                this.threadId = threadId;
                this.byteBuffer = ByteBuffer.allocate(bufferSize);
            }
        }

        var channels = new ArrayList<DatagramChannel>();
        try (final Selector selector = Selector.open()) {
            var socket = new InetSocketAddress(host, port);
            for (int i = 0; i < threads; i++) {
                DatagramChannel channel = DatagramChannel.open();
                channel.configureBlocking(false);
                channel.connect(socket);
                channel.register(selector, SelectionKey.OP_WRITE, new MetaInfo(i, channel.socket().getReceiveBufferSize()));
                channels.add(channel);
            }

            while (!selector.keys().isEmpty() && !Thread.interrupted()) {
                final var selectedKeys = selector.selectedKeys();
                selector.select(SOCKET_TIMEOUT);
                if (selectedKeys.isEmpty()) selector.keys().forEach(k -> k.interestOps(SelectionKey.OP_WRITE));
                for (final Iterator<SelectionKey> it = selectedKeys.iterator(); it.hasNext(); ) {
                    final var key = it.next();
                    try {
                        if (key.isValid()) {
                            final var channel = (DatagramChannel) key.channel();
                            final var metaInfo = (MetaInfo) key.attachment();
                            if (key.isReadable()) {
                                channel.receive(metaInfo.byteBuffer.clear());
                                final String response = StandardCharsets.UTF_8.
                                        decode(metaInfo.byteBuffer.flip()).toString();
                                if (response.contains(joinRequest(prefix, metaInfo.threadId, metaInfo.requestId))) {
                                    System.out.printf("Received: %s%n", response);
                                    metaInfo.requestId++;
                                }
                                key.interestOps(SelectionKey.OP_WRITE);
                                if (metaInfo.requestId >= requests) {
                                    channel.close();
                                }
                            } else {
                                final var message = joinRequest(
                                        prefix,
                                        metaInfo.threadId,
                                        metaInfo.requestId
                                );
                                System.out.printf("Sending: %s%n", message);
                                channel.send(
                                        ByteBuffer.wrap(message.getBytes(StandardCharsets.UTF_8)),
                                        socket
                                );
                                key.interestOps(SelectionKey.OP_READ);
                            }
                        }
                    } finally {
                        it.remove();
                    }
                }
            }
        } catch (final IOException e) {
            System.err.printf(
                    "Communication error on host: %s and port %s%n. Exception: %s%n",
                    host,
                    port,
                    e.getLocalizedMessage()
            );
        } finally {
            channels.forEach(c -> {
                try {
                    c.close();
                } catch (final IOException e) {
                    System.err.printf(
                            "Exception while closing channel: %s%n On host: %s, port: %s%n",
                            e.getLocalizedMessage(),
                            host,
                            port
                    );
                }
            });
        }
    }
}
