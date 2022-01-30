package msqueue;

import org.junit.Test;

import java.util.ArrayDeque;
import java.util.Random;

import static org.junit.Assert.assertEquals;

public class FunctionalTest {
    private static Random R = new Random(0);

    @Test
    public void test() {
        Queue queue = new MSQueue();
        java.util.Queue<Integer> javaQueue = new ArrayDeque<>();
        for (int i = 0; i < 1_000_000; i++) {
            int op = R.nextInt(3);
            int x = R.nextInt(30);
            switch (op) {
            case 0:
                // enqueue
                javaQueue.add(x);
                queue.enqueue(x);
                break;
            case 1:
                // peek
                if (javaQueue.isEmpty()) assertEquals(Integer.MIN_VALUE, queue.peek());
                else assertEquals((int) javaQueue.peek(), queue.peek());
                break;
            case 2:
                // dequeue
                if (javaQueue.isEmpty()) assertEquals(Integer.MIN_VALUE, queue.dequeue());
                else assertEquals((int) javaQueue.poll(), queue.dequeue());
                break;
            }
        }
    }
}