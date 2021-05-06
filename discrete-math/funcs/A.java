import java.io.*;
import java.util.*;
import java.util.function.Function;
import java.util.stream.IntStream;

public class A {

    private static final int MOD = 998_244_353;

    public static void main(String[] args) {
        InputReader in = new InputReader(System.in);
        PrintWriter out = new PrintWriter(System.out);

        int n = in.nextInt();
        int m = in.nextInt();

        final Long[] p = new Long[n + 1];
        final Long[] q = new Long[m + 1];

        IntStream.rangeClosed(0, n).forEach(i -> p[i] = in.nextLong());
        IntStream.rangeClosed(0, m).forEach(i -> q[i] = in.nextLong());

        final List<Long> list = new ArrayList<>(Math.max(p.length, q.length));
        Collections.addAll(list, p.length > q.length ? p : q);
        for (int i = 0; i < Math.min(q.length, p.length); i++) {
            list.set(i, (p[i] + q[i]) % MOD);
        }
        clearAndWrite(out, list);

        list.clear();

        IntStream.rangeClosed(0, p.length + q.length).forEach(i -> list.add(0L));

        final Function<Long[], Function<Integer, Long>> compareAndGetElement = lst -> index ->
                index >= lst.length ? 0L : lst[index];

        for (int i = 0; i < list.size(); i++) {
            for (int j = 0; j <= i; j++) {
                list.set(i, (MOD + list.get(i) + (
                        MOD + compareAndGetElement.apply(p).apply(j) *
                                compareAndGetElement.apply(q).apply(i - j))
                        % MOD)
                        % MOD);
            }
        }

        clearAndWrite(out, list);

        final Long[] dp = IntStream.range(0, 1000).mapToObj(i -> 0L).toArray(Long[]::new);
        dp[0] = 1L;

        for (int i = 0; i < 1000; i++) {
            for (int j = 1; j <= i; j++) {
                dp[i] = (dp[i] -
                        (compareAndGetElement.apply(q).apply(j) * compareAndGetElement.apply(dp).apply(i - j)) % MOD)
                        % MOD;
            }
        }
        
        final Long[] ans = IntStream.range(0, 1000).mapToObj(i -> 0L).toArray(Long[]::new);

        for (int i = 0; i < ans.length; i++) {
            for (int j = 0; j <= i; j++) {
                ans[i] = (MOD + ans[i] + (MOD + compareAndGetElement.apply(p).apply(j) * compareAndGetElement.apply(dp).apply(i - j)) % MOD) % MOD;
            }
        }

        Arrays.stream(ans).forEach(el -> out.print(el + " "));
        out.close();
    }

    private static void clearAndWrite(final PrintWriter out, final List<Long> list) {
        while (list.get(list.size() - 1) == 0 && list.size() > 1) {
            list.remove(list.size() - 1);
        }

        out.println((list.size() == 1 && list.get(0) == 0 ? 0 : list.size() - 1));

        list.forEach(el -> out.print(el + " "));
        out.println();
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
}
