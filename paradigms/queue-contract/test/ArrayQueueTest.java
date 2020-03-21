package test;

import queue.ArrayQueue;

public class ArrayQueueTest {
    public static void main(String[] args) {
        ArrayQueue queue = new ArrayQueue();

        // add 5 elements
        for (int i = 0; i < 5; i++) {
            queue.enqueue(i);
        }

        // get queue size
        // res = 5
        System.out.println("Queue size: " + queue.size());


        // get top element
        // res = 0
        System.out.println("Top element: " + queue.element());

        // pop 3 elements
        for (int i = 0; i < 3; ++i) {
            queue.dequeue();
        }

        // get top element
        // res = 3
        System.out.println("Top element: " + queue.element());

        if (queue.isEmpty()) {
            System.out.println("Empty queue");
        } else {
            System.out.println("Not empty queue");
        }

        // clear all
        queue.clear();

        if (queue.isEmpty()) {
            System.out.println("Empty queue");
        }
        // dequeue from empty queue
        try {
            queue.dequeue();
        } catch (AssertionError e) {
            System.out.println(e.getMessage());
        }
        // add null element
        try {
            queue.enqueue(null);
        } catch (AssertionError e) {
            System.out.println(e.getMessage());
        }
        // get top element from empty queue
        try {
            System.out.println(queue.element());
        } catch (AssertionError e) {
            System.out.println(e.getMessage());
        }

        // push 5 elements
        for (int i = 0; i < 5; i++) {
            queue.push(i);
        }

        // peek
        // res = 0
        System.out.println(queue.peek());

        // remove
        // res = 0
        System.out.println(queue.remove());

        // clear
        queue.clear();

        // peek from empty queue
        try {
            System.out.println(queue.peek());
        } catch (AssertionError e) {
            System.out.println(e.getMessage());
        }

    }
}
