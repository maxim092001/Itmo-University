package mnkGame;

import java.io.PrintStream;
import java.util.Scanner;

public class HumanPlayer implements Player {

    private Scanner inputScanner;
    private PrintStream outputStream;
    private final String id;

    public HumanPlayer(final String id) {
        this.inputScanner = new Scanner(System.in);
        this.outputStream = System.out;
        this.id = id;
    }

    private int readInputData() {
        while (!inputScanner.hasNextInt()) {
            inputScanner.next();
            log("Invalid input");
        }
        return inputScanner.nextInt();
    }


    @Override
    public Move makeMove(final Position position, final Cell cell) {
        while (true) {
            log("Position");
            log(position.toString());
            log(cell + "'s move");
            log("Enter row and column");
            final Move move = new Move(readInputData(), readInputData(), cell);
            if (position.isValidToMakeMove(move)) {
                return move;
            }
            log("Move " + move + " is invalid");
        }
    }

    @Override
    public String getId() {
        return this.id;
    }

    private void log(final String s) {
        outputStream.println(s);
    }
}
