package info.kgeorgiy.ja.grankin.hello;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.TimeUnit;

public class UDPUtils {

    public static void terminationWait(final ExecutorService pool, final long terminationSeconds) {
        pool.shutdown();
        try {
            if (!pool.awaitTermination(terminationSeconds, TimeUnit.SECONDS)) {
                System.err.println("Time out for thread pool");
            }
        } catch (InterruptedException e) {
            System.err.printf("Interrupted exception: %s%n", e.getLocalizedMessage());
        }
    }
}
