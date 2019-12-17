package mnkGame;

public class Game {
    private Player firstPlayer;
    private Player secondPlayer;

    public Game(final Player firstPlayer, final Player secondPlayer) {
        this.firstPlayer = firstPlayer;
        this.secondPlayer = secondPlayer;
    }

    public int play(final Board board) {
        while (true) {
            final int result1 = move(board, firstPlayer, 1);
            if (result1 != -1) {
                return result1;
            }
            final int result2 = move(board, secondPlayer, 2);
            if (result2 != -1) {
                return result2;
            }
        }
    }

    private int move(final Board board, final Player player, final int num) {
        final Move move = player.makeMove(board.getPosition(), board.getCell());
        final Result result = board.makeMove(move);

        if (result == Result.WIN) {
            return num;
        } else if (result == Result.LOSE) {
            return 3 - num;
        } else if (result == Result.DRAW) {
            return 0;
        } else {
            return -1;
        }
    }
}
