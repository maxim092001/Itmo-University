package info.kgeorgiy.ja.grankin.concurrent;

import info.kgeorgiy.java.advanced.mapper.ParallelMapper;

import java.util.*;
import java.util.function.Function;

public class ParallelMapperImpl implements ParallelMapper {

    private static final int QUEUE_MAX_SIZE = 1000;
    private final List<Thread> threadWorkers;
    private final Queue<Runnable> tasks;

    private static class ResultConcurrentList<T> {
        private final List<T> list;
        private int counter = 0;

        public ResultConcurrentList(final int listSize) {
            this.list = new ArrayList<>(Collections.nCopies(listSize, null));
        }

        private synchronized void checkAndNotify() {
            if (++counter >= list.size()) {
                notify();
            }
        }

        public void setListElement(final int position, final T element) {
            list.set(position, element);
            checkAndNotify();
        }

        public synchronized List<T> getList() throws InterruptedException {
            while (counter < list.size()) wait();
            return list;
        }
    }

    public ParallelMapperImpl(final int threads) {
        this.tasks = new ArrayDeque<>();
        this.threadWorkers = new ArrayList<>();

        for (int i = 0; i < threads; i++) {
            final var newThreadWorker = new Thread(threadWorkerAction());
            threadWorkers.add(newThreadWorker);
            newThreadWorker.start();
        }
    }

    // :NOTE: ignored exception's information
    @Override
    public <T, R> List<R> map(final Function<? super T, ? extends R> f, final List<? extends T> args) throws InterruptedException {
        final var result = new ResultConcurrentList<R>(args.size());
        for (int i = 0; i < args.size(); i++) {
            final int finalI = i;
            addTaskToQuery(() -> result.setListElement(finalI, f.apply(args.get(finalI))));
        }
        return result.getList();
    }

    private void addTaskToQuery(final Runnable task) throws InterruptedException {
        synchronized (tasks) {
            while (tasks.size() == QUEUE_MAX_SIZE) task.wait();
            tasks.add(task);
            tasks.notifyAll();
        }
    }

    @Override
    public void close() {
        threadWorkers.forEach(thread -> {
            thread.interrupt();
            try {
                thread.join();
            } catch (InterruptedException e) {
                // logs System.err.println("Exception in thread joining: " + e.getLocalizedMessage());
            }
        });
    }

    private Runnable threadWorkerAction() {
        return () -> {
            try {
                while (!Thread.interrupted()) {
                    final Runnable task;
                    synchronized (tasks) {
                        while (tasks.isEmpty()) tasks.wait();
                        task = tasks.poll();
                        tasks.notifyAll();
                    }
                    task.run();
                }
            } catch (final InterruptedException e) {
                // logs System.err.println("Exception in thread running: " + e.getLocalizedMessage());
            } finally {
                Thread.currentThread().interrupt();
            }
        };
    }
}
