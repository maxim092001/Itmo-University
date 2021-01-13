import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
 
/**
 * @author Grankin Maxim (maximgran@gmail.com) at 10:32 23.12.2020
 */
public class D {
 
 
    public static void main(String[] args) throws FileNotFoundException {
        InputReader in = new InputReader(new FileInputStream("check.in"));
        PrintStream out = new PrintStream(new FileOutputStream("check.out"));
 
        int n = in.nextInt();
        int m = in.nextInt();
 
        Set<List<Integer>> mySet = new HashSet<>();
        List<List<Integer>> lst = IntStream
                .range(0, m)
                .mapToObj(i -> new ArrayList<Integer>())
                .collect(Collectors.toList());
 
 
        int[] counter = new int[m];
        boolean firstAxiom = false;
        for (int i = 0; i < m; i++) {
            counter[i] = in.nextInt();
 
            for (int j = 0; j < counter[i]; j++) {
                int x = in.nextInt();
                lst.get(i).add(x);
            }
 
            lst.get(i).sort(Integer::compareTo);
 
            mySet.add(lst.get(i));
            if (counter[i] == 0) {
                firstAxiom = true;
            }
        }
 
        if (!firstAxiom || !secondAxiom(m, counter, mySet, lst) || !thirdAxiom(m, n, counter, lst, mySet)) {
            out.println("NO");
        } else {
            out.println("YES");
        }
 
        out.close();
    }
 
    private static boolean secondAxiom(
            int m,
            int[] counter,
            Set<List<Integer>> mySet,
            List<List<Integer>> lst
    ) {
        for (int i = 0; i < m; i++) {
            for (int j = 1; j < (1 << counter[i]); j++) {
                final int jFinal = j;
                final int iFinal = i;
                List<Integer> temp = IntStream.range(0, counter[i])
                        .filter(k -> (1 & (jFinal >> k)) == 1)
                        .mapToObj(k -> lst.get(iFinal).get(k))
                        .sorted(Integer::compareTo)
                        .collect(Collectors.toList());
 
                if (!mySet.contains(temp)) {
                    return false;
                }
            }
        }
        return true;
    }
 
    private static boolean thirdAxiom(
            int m,
            int n,
            int[] counter,
            List<List<Integer>> lst,
            Set<List<Integer>> mySet
    ) {
        for (int i = 0; i < m; i++) {
            for (int j = 0; j < m; j++) {
                if (counter[i] <= counter[j]) {
                    continue;
                }
                final int jFinal = j;
                final int iFinal = i;
 
                Optional<List<Integer>> thirdAxiom = IntStream.rangeClosed(1, n)
                        .filter(k -> !(!lst.get(iFinal).contains(k) || lst.get(jFinal).contains(k)))
                        .mapToObj(k -> {
                            List<Integer> temp = new ArrayList<>(lst.get(jFinal));
                            temp.add(k);
                            temp.sort(Integer::compareTo);
                            return temp;
                        }).filter(mySet::contains).findFirst();
 
                if (thirdAxiom.isEmpty()) {
                    return false;
                }
            }
        }
        return true;
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
 
        public long nextLong() {
            return Long.parseLong(next());
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