package mnkGame;

import java.io.PrintStream;
import java.util.HashMap;
import java.util.Map;

public class Series {

    private Player firstPlayer, secondPlayer;
    private PrintStream outputStream;

    public Series(final Player firstPlayer, final Player secondPlayer) {
        this.firstPlayer = firstPlayer;
        this.secondPlayer = secondPlayer;
        outputStream = System.out;
    }

    public void playSeries(Board board, int rounds) {

        Map<String, Integer> seriesCount = new HashMap<>(Map.of(
                firstPlayer.getId(), 0,
                secondPlayer.getId(), 0
        ));

        Game game = new Game(firstPlayer, secondPlayer);

        while (seriesCount.get(firstPlayer.getId()) + seriesCount.get(secondPlayer.getId()) != rounds) {
            int result = game.play(board);

            log("Position:");
            log(board.toString());
            board.clear();

            if (result == 1) {
                log("Player " + firstPlayer.getId() + " won round");
                seriesCount.replace(firstPlayer.getId(), seriesCount.get(firstPlayer.getId()) + 1);
            } else if (result == 2) {
                log("Player " + secondPlayer.getId() + " won round");
                seriesCount.replace(secondPlayer.getId(), seriesCount.get(secondPlayer.getId()) + 1);
            }
            swapPlayers();
        }

        if (seriesCount.get(firstPlayer.getId()).equals(seriesCount.get(secondPlayer.getId()))) {
            log("DRAW");
        } else {
            if (seriesCount.get(firstPlayer.getId()) > seriesCount.get(secondPlayer.getId())) {
                log("Player " + firstPlayer.getId() + " won series");
            } else {
                log("Player " + secondPlayer.getId() + " won series");
            }
        }
    }

    private void swapPlayers() {
        Player temp = firstPlayer;
        firstPlayer = secondPlayer;
        secondPlayer = temp;
    }

    private void log(String s) {
        outputStream.println(s);
    }
}
