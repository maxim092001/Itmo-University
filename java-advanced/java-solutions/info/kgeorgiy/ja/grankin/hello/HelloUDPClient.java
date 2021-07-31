package info.kgeorgiy.ja.grankin.hello;

import java.io.IOException;
import java.net.*;
import java.nio.charset.StandardCharsets;
import java.util.concurrent.Executors;
import java.util.stream.IntStream;

public class HelloUDPClient extends AbstractClient {

    public static void main(final String... args) {
        new HelloUDPClient().parseArgsAndRunClient(args);
    }

    @Override
    public void run(final String host, final int port, final String prefix, final int threads, final int requests) {
        try {
            final var socketAddress = new InetSocketAddress(InetAddress.getByName(host), port);
            final var pool = Executors.newFixedThreadPool(threads);
            IntStream.range(0, threads)
                    .forEach(i ->
                            pool.submit(() ->
                                    makeAndSendMessage(host, port, socketAddress, prefix, i, requests)
                            )
                    );
            UDPUtils.terminationWait(pool, REQUEST_TIMEOUT * threads * requests);
        } catch (final UnknownHostException e) {
            System.err.printf("Unknown host exception: %s%n", e.getLocalizedMessage());
        } catch (final SecurityException e) {
            System.err.printf("Security manager exception: %s%n", e.getLocalizedMessage());
        }
    }

    private void makeAndSendMessage(
            final String host,
            final int port,
            final SocketAddress socketAddress,
            final String prefix,
            final int thread,
            final int requests
    ) {
        try (final DatagramSocket datagramSocket = new DatagramSocket()) {
            datagramSocket.setSoTimeout(SOCKET_TIMEOUT);
            final var bufSize = datagramSocket.getSendBufferSize();
            final var packet = new DatagramPacket(new byte[bufSize], bufSize, socketAddress);
            IntStream.range(0, requests)
                    .mapToObj(id -> joinRequest(prefix, thread, id))
                    .forEach(message -> sendMessage(message, datagramSocket, packet, bufSize));
        } catch (final SocketException e) {
            System.err.printf("Socket exception for: %s on %d %n%s%n", host, port, e.getLocalizedMessage());
        }
    }

    private void sendMessage(
            final String message,
            final DatagramSocket socket,
            final DatagramPacket packet,
            final int bufSize
    ) {
        System.out.printf("Sending: %s%n", message);
        while (true) {
            try {
                packet.setData(message.getBytes(StandardCharsets.UTF_8));
                socket.send(packet);
                packet.setData(new byte[bufSize]);
                socket.receive(packet);
                String responseMessage = new String(
                        packet.getData(),
                        packet.getOffset(),
                        packet.getLength(),
                        StandardCharsets.UTF_8
                );
                if (responseMessage.contains(message)) {
                    System.out.printf("Received: %s%n", responseMessage);
                    break;
                }
            } catch (final IOException e) {
                System.err.printf(
                        "Send or receive exception for message: %s%n%s%n",
                        message,
                        e.getLocalizedMessage()
                );
            }
        }
    }
}
