package info.kgeorgiy.ja.grankin.hello;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.SocketException;
import java.nio.charset.StandardCharsets;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class HelloUDPServer extends AbstractServer {

    private DatagramSocket datagramSocket;
    private ExecutorService main;

    public static void main(final String[] args) {
        new HelloUDPServer().parseAndStart(args);
    }

    @Override
    public void start(final int port, final int threads) {
        try {
            datagramSocket = new DatagramSocket(port);
            pool = Executors.newFixedThreadPool(threads);
            main = Executors.newSingleThreadExecutor();
            main.submit(() -> {
                try {
                    final var buffSize = datagramSocket.getReceiveBufferSize();
                    while (!Thread.interrupted()) {
                        final var packet = new DatagramPacket(new byte[buffSize], buffSize);
                        datagramSocket.receive(packet);

                        pool.submit(() -> respond(packet, port));
                    }
                } catch (final IOException e) {
                    System.err.printf("IO exception on port: %d%n%s%n", port, e.getLocalizedMessage());
                }
            });
        } catch (final SocketException e) {
            System.err.printf("Socket exception on port: %d%n%s%n", port, e.getLocalizedMessage());
        } catch (final SecurityException e) {
            System.err.printf("Security exception manager on port: %d%n%s%n", port, e.getLocalizedMessage());
        }
    }

    private void respond(final DatagramPacket packet, final int port) {
        packet.setData(helloResponse(new String(
                packet.getData(),
                packet.getOffset(),
                packet.getLength(),
                StandardCharsets.UTF_8
        )).getBytes(StandardCharsets.UTF_8));
        try {
            datagramSocket.send(packet);
        } catch (final IOException e) {
            System.err.printf("IO exception on port: %d%n%s%n", port, e.getLocalizedMessage());
        }
    }

    @Override
    public void close() {
        datagramSocket.close();
        UDPUtils.terminationWait(main, THREAD_TERMINATION_SECONDS);
        UDPUtils.terminationWait(pool, THREAD_TERMINATION_SECONDS);
    }
}
