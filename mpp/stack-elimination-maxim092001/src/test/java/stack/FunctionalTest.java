package stack;

import org.junit.Test;

import java.util.Random;
import java.util.Stack;

import static org.junit.Assert.assertEquals;

public class FunctionalTest {
    private static Random R = new Random(0);

    @Test
    public void test() {
        stack.Stack myStack = new StackImpl();
        Stack<Integer> javaStack = new Stack<>();
        for (int i = 0; i < 1_000_000; i++) {
            int op = R.nextInt(2);
            switch (op) {
            case 0:
                // push
                javaStack.push(i);
                myStack.push(i);
                break;
            case 1:
                // pop
                if (javaStack.isEmpty()) {
                    assertEquals(Integer.MIN_VALUE, myStack.pop());
                } else {
                    assertEquals((int) javaStack.pop(), myStack.pop());
                }
                break;
            }
        }
    }
}