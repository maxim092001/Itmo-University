import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;
import java.util.TreeMap;
 
/**
 * @author Grankin Maxim (maximgran@gmail.com) at 10:32 23.12.2020
 */
public class A {
 
    public static void main(String[] args) throws FileNotFoundException {
        InputReader in = new InputReader(new FileInputStream("schedule.in"));
        PrintStream out = new PrintStream(new FileOutputStream("schedule.out"));
 
        int n = in.nextInt();
 
        long ans = 0;
 
        List<Task> tasks = new ArrayList<>();
 
 
        for (int i = 0; i < n; i++) {
            long d = in.nextInt();
            long w = in.nextInt();
 
            tasks.add(new Task(d, w));
            ans += w;
        }
 
        tasks.sort((a, b) -> {
            int res;
            if (a.d < b.d) {
                res = -1;
            } else if (a.d > b.d) {
                res = 1;
            } else {
                res = Long.compare(a.w, b.w);
            }
            return res;
        });
 
        Map<Long, Long> calc = calculate(tasks);
 
        out.println(ans - calc.entrySet().stream().mapToLong(entry -> entry.getValue() * entry.getKey())
                .reduce(0, Long::sum));
        out.close();
    }
 
 
    private static Map<Long, Long> calculate(final List<Task> tasks) {
        long t = 1;
 
        TreeMap<Long, Long> res = new TreeMap<>();
 
        for (Task task : tasks) {
            if (task.d == 0) {
                continue;
            }
 
            res.put(task.w, res.getOrDefault(task.w, 0L) + 1);
 
            if (task.d >= t) {
                t++;
            } else {
                res.remove(res.firstKey());
            }
        }
 
        return res;
    }
 
    private static class Task {
        long d;
        long w;
 
        public Task(final long d, final long w) {
            this.d = d;
            this.w = w;
        }
 
    }
 
    private static class InputReader {
        public BufferedReader reader;
        public StringTokenizer tokenizer;
 
        public InputReader(InputStream stream) {
            reader = new BufferedReader(new InputStreamReader(stream), 32768);
            tokenizer = null;
        }
 
        public String next() {
            while (tokenizer == null || !tokenizer.hasMoreTokens()) {
                try {
                    tokenizer = new StringTokenizer(reader.readLine());
                } catch (IOException e) {
                    throw new RuntimeException(e);
                }
            }
            return tokenizer.nextToken();
        }
 
        public int nextInt() {
            return Integer.parseInt(next());
        }
 
    }
 
    private static class IntList {
 
        private int[] array;
        private int size;
 
        public IntList() {
            init();
        }
 
        private void init() {
            array = new int[8];
            size = 0;
        }
 
        public void add(int value) {
            addToList(value);
        }
 
        private void addToList(int value) {
            if (size == array.length) {
                array = Arrays.copyOf(array, array.length + (array.length >> 1));
            }
            array[size++] = value;
        }
 
        public int size() {
            return this.size;
        }
 
        public int get(int ind) {
            return array[ind];
        }
 
    }
}