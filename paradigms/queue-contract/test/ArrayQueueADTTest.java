package test;

import queue.ArrayQueueADT;

public class ArrayQueueADTTest {
    public static void main(String[] args) {

        ArrayQueueADT queue = null;

        // using null queue
        try {
            ArrayQueueADT.enqueue(queue, 12);
        } catch (AssertionError e) {
            System.out.println(e.getMessage());
        }

        queue = new ArrayQueueADT();

        // add 5 elements
        for (int i = 0; i < 5; i++) {
            ArrayQueueADT.enqueue(queue, i);
        }

        // get queue size
        // res = 5
        System.out.println("Queue size: " + ArrayQueueADT.size(queue));

        // get top element
        // res = 0
        System.out.println("Top element: " + ArrayQueueADT.element(queue));

        // pop 3 elements
        for (int i = 0; i < 3; ++i) {
            ArrayQueueADT.dequeue(queue);
        }

        // get top element
        // res = 3
        System.out.println("Top element: " + ArrayQueueADT.element(queue));


        if (ArrayQueueADT.isEmpty(queue)) {
            System.out.println("Empty queue");
        } else {
            System.out.println("Not empty queue");
        }

        // clear all
        ArrayQueueADT.clear(queue);

        if (ArrayQueueADT.isEmpty(queue)) {
            System.out.println("Empty queue");
        } else {
            System.out.println("Not empty queue");
        }
        // dequeue from empty queue
        try {
            ArrayQueueADT.dequeue(queue);
        } catch (AssertionError e) {
            System.out.println(e.getMessage());
        }
        // add null element
        try {
            ArrayQueueADT.enqueue(queue, null);
        } catch (AssertionError e) {
            System.out.println(e.getMessage());
        }
        // get top element from empty queue
        try {
            System.out.println(ArrayQueueADT.element(queue));
        } catch (AssertionError e) {
            System.out.println(e.getMessage());
        }

        // push 5 elements
        for (int i = 0; i < 5; i++) {
            ArrayQueueADT.push(queue, i);
        }

        // peek
        // res = 0
        System.out.println(ArrayQueueADT.peek(queue));

        // remove
        // res = 0
        System.out.println(ArrayQueueADT.remove(queue));

        // clear
        ArrayQueueADT.clear(queue);

        // peek from empty queue
        try {
            System.out.println(ArrayQueueADT.peek(queue));
        } catch (AssertionError e) {
            System.out.println(e.getMessage());
        }
    }
}
