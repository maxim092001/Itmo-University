package mnkGame;

import java.util.Scanner;

public class Main {
    public static void main(String[] args) {
        final Series series = new Series(new HumanPlayer("1"), new HumanPlayer("2"));
        int n, m, k;

        Scanner in = new Scanner(System.in);

        m = in.nextInt();
        n = in.nextInt();
        k = in.nextInt();

        series.playSeries(new PlayerBoard(m, n, k), 2);
    }
}
