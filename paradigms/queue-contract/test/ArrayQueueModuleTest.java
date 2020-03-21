package test;

import queue.ArrayQueueModule;

public class ArrayQueueModuleTest {
    public static void main(String[] args) {

        // add 5 elements
        for (int i = 0; i < 5; i++) {
            ArrayQueueModule.enqueue(i);
        }

        // get queue size
        // res = 5
        System.out.println("Queue size: " + ArrayQueueModule.size());

        // get top element
        // res = 0
        System.out.println("Top element: " + ArrayQueueModule.element());

        // pop 3 elements
        for (int i = 0; i < 3; ++i) {
            ArrayQueueModule.dequeue();
        }

        // get top element
        // res = 3
        System.out.println("Top element: " + ArrayQueueModule.element());

        if (ArrayQueueModule.isEmpty()) {
            System.out.println("Empty queue");
        } else {
            System.out.println("Not empty queue");
        }

        // clear all
        ArrayQueueModule.clear();

        if (ArrayQueueModule.isEmpty()) {
            System.out.println("Empty queue");
        }
        // dequeue from empty queue
        try {
            ArrayQueueModule.dequeue();
        } catch (AssertionError e) {
            System.out.println(e.getMessage());
        }
        // add null element
        try {
            ArrayQueueModule.enqueue(null);
        } catch (AssertionError e) {
            System.out.println(e.getMessage());
        }
        // get top element from empty queue
        try {
            System.out.println(ArrayQueueModule.element());
        } catch (AssertionError e) {
            System.out.println(e.getMessage());
        }

        // push 5 elements
        for (int i = 0; i < 5; i++) {
            ArrayQueueModule.push(i);
        }

        // peek
        // res = 0
        System.out.println(ArrayQueueModule.peek());

        // remove
        // res = 0
        System.out.println(ArrayQueueModule.remove());

        // clear
        ArrayQueueModule.clear();

        // peek from empty queue
        try {
            System.out.println(ArrayQueueModule.peek());
        } catch (AssertionError e) {
            System.out.println(e.getMessage());
        }
    }
}
