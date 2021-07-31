package info.kgeorgiy.ja.grankin.crawler;

import info.kgeorgiy.java.advanced.crawler.*;

import java.io.IOException;
import java.net.MalformedURLException;
import java.util.*;
import java.util.concurrent.*;
import java.util.function.BiFunction;

public class WebCrawler implements Crawler {
    private final Downloader downloader;
    private final int perHost;
    private final ExecutorService extractors;
    private final ExecutorService downloaders;
    private final ConcurrentMap<String, HostWorker> hostsMap = new ConcurrentHashMap<>();

    public WebCrawler(
            final Downloader downloader,
            final int downloaders,
            final int extractors,
            final int perHost
    ) {
        this.downloader = downloader;
        this.downloaders = Executors.newFixedThreadPool(downloaders);
        this.extractors = Executors.newFixedThreadPool(extractors);
        this.perHost = perHost;
    }

    @Override
    public Result download(final String url, final int depth) {
        return new DownloadWorker().downloadAndGetResult(url, depth);
    }

    @Override
    public void close() {
        extractors.shutdown();
        downloaders.shutdown();
        if (!(extractors.isShutdown() && downloaders.isShutdown())) {
            System.err.println("Could not shutdown pools");
        }
    }


    public static void main(String[] args) {
        if (args == null || args.length == 0 || Arrays.stream(args).anyMatch(Objects::isNull)) {
            System.err.println("Expected not null arguments");
        } else {
            final BiFunction<String[], Integer, Integer> argValue =
                    (arr, idx) -> idx >= arr.length ? 1 : Integer.parseInt(arr[idx]);
            try {
                try (Crawler crawler = new WebCrawler(
                        new CachingDownloader(),
                        argValue.apply(args, 2),
                        argValue.apply(args, 3),
                        argValue.apply(args, 4)
                )) {
                    crawler.download(args[0], argValue.apply(args, 1));
                }
            } catch (final NumberFormatException e) {
                System.err.printf("Expected numbers in arguments but found: %s \n", e.getMessage());
            } catch (final IOException e) {
                System.err.printf("Exception while initializing: %s ", e.getMessage());
            }
        }
    }

    private final class DownloadWorker {

        private final Map<String, IOException> errors = new ConcurrentHashMap<>();
        private final Set<String> downloaded = ConcurrentHashMap.newKeySet();
        private final Set<String> cached = ConcurrentHashMap.newKeySet();
        private final Phaser phaser = new Phaser(1);

        private void download(final String url, final int depth, final Queue<String> queue) {
            try {
                final String hostName = URLUtils.getHost(url);
                final var hostWorker = hostsMap.computeIfAbsent(hostName, name -> HostWorker.of(perHost, downloaders));
                phaser.register();
                hostWorker.addTask(() -> {
                    try {
                        final var doc = downloader.download(url);
                        downloaded.add(url);
                        if (depth > 1) {
                            phaser.register();
                            extractors.submit(extractorsPoolAction(doc, url, queue));
                        }
                    } catch (final IOException e) {
                        errors.put(url, e);
                    } finally {
                        phaser.arriveAndDeregister();
                        hostWorker.runNewTask();
                    }
                });
            } catch (final MalformedURLException e) {
                errors.put(url, e);
            }
        }

        private Runnable extractorsPoolAction(final Document doc, final String url, final Queue<String> queue) {
            return () -> {
                try {
                    doc.extractLinks().forEach(link -> {
                        if (!cached.contains(link)) {
                            cached.add(link);
                            queue.add(link);
                        }
                    });
                } catch (final IOException e) {
                    System.err.printf("Exception while extracting links from doc by url: %s \n", url);
                } finally {
                    phaser.arriveAndDeregister();
                }
            };
        }

        public Result downloadAndGetResult(final String url, final int depth) {
            cached.add(url);
            final Queue<String> prevUrls = new ConcurrentLinkedDeque<>();
            final Queue<String> curUrls = new ConcurrentLinkedDeque<>();
            prevUrls.add(url);
            for (int i = 0; i < depth; i++) {
                for (final String prevUrl : prevUrls) {
                    download(prevUrl, depth - i, curUrls);
                }
                phaser.arriveAndAwaitAdvance();
                prevUrls.clear();
                prevUrls.addAll(curUrls);
                curUrls.clear();
            }
            return new Result(new ArrayList<>(downloaded), errors);
        }
    }


    private static final class HostWorker {
        private final Queue<Runnable> nonWorkingTasks = new ConcurrentLinkedDeque<>();
        private int working;
        private final int perHost;
        private final ExecutorService downloaders;

        private HostWorker(final int perHost, final ExecutorService downloaders) {
            this.perHost = perHost;
            this.downloaders = downloaders;
        }

        public static HostWorker of(final int perHost, final ExecutorService downloaderPool) {
            return new HostWorker(perHost, downloaderPool);
        }

        private void addTask(final Runnable task) {
            if (working >= perHost) {
                nonWorkingTasks.add(task);
            } else {
                downloaders.submit(task);
                working++;
            }
        }

        private void runNewTask() {
            var task = nonWorkingTasks.poll();
            if (Objects.nonNull(task)) downloaders.submit(task);
            else working--;
        }
    }
}
