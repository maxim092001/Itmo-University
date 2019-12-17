package mnkGame;

import java.util.Arrays;
import java.util.Map;

public class MnkBoard implements Position, Board {

    private int k;
    protected Cell[][] cells;
    private Cell turn;
    private int numberOfEmptyCells;

    private static final Map<Cell, Character> SYMBOLS = Map.of(
            Cell.X, 'X',
            Cell.O, 'O',
            Cell.E, '.'
    );

    public MnkBoard(final int m, final int n, final int k) {
        if (notPositiveSizes(m, n, k)) {
            throw new IllegalArgumentException("Dimensions must be positive numbers");
        }
        if (unrealGame(m, n, k)) {
            throw new IllegalArgumentException("Unreal game. \'k\' has to be less (or equals) m or n");
        }
        fillBoard(m, n, k);
    }

    private void setNextTurn() {
        this.turn = (this.turn == Cell.X ? Cell.O : Cell.X);
    }

    private boolean unrealGame(int m, int n, int k) {
        return k > Math.max(m, n);
    }

    private boolean notPositiveSizes(int m, int n, int k) {
        return m <= 0 || n <= 0 || k <= 0;
    }

    private int checkCells(int iMove, int jMove, Move move) {

        int cnt = 0;
        int i = move.getRow();
        int j = move.getColumn();

        while (i >= 0 && i < cells.length && j >= 0 && j < cells[0].length && cells[i][j] == move.getValueInCell()) {
            i += iMove;
            j += jMove;
            cnt++;
        }

        return cnt;
    }

    @Override
    public void clear() {
        fillBoard(cells.length, cells[0].length, k);
    }

    private boolean won(Move move, int i, int j) {
        return checkCells(i, j, move) + checkCells(-i, -j, move) - 1 >= k;
    }

    @Override
    public Result makeMove(Move move) {
        if (!isValidToMakeMove(move)) {
            return Result.LOSE;
        }

        setNextTurn();

        cells[move.getRow()][move.getColumn()] = move.getValueInCell();
        numberOfEmptyCells--;

        if (won(move, 0, 1) || won(move, 1, 0) || won(move, 1, 1) || won(move, 1, -1)) {
            return Result.WIN;
        }

        for (int i = -1; i <= 0; i++) {
            for (int j = -1; j <= 1; j++) {
                if (i < 0 || j < 0) {
                    if (checkCells(i, j, move) + checkCells(-i, -j, move) - 1 >= k) {
                        return Result.WIN;
                    }
                }
            }
        }

        if (numberOfEmptyCells == 0) {
            return Result.DRAW;
        }

        return Result.UNKNOWN;
    }

    @Override
    public Position getPosition() {
        return new UnrealBoard(cells);
    }

    @Override
    public Cell getCell() {
        return turn;
    }

    @Override
    public void fillBoard(int m, int n, int k) {
        this.k = k;
        this.turn = Cell.X;
        this.numberOfEmptyCells = m * n;

        this.cells = new Cell[m][n];
        for (Cell[] cellArray : cells) {
            Arrays.fill(cellArray, Cell.E);
        }
    }

    @Override
    public boolean isValidToMakeMove(Move move) {
        int row = move.getRow();
        int column = move.getColumn();
        return row >= 0 && column >= 0 && row < cells.length
                && column < cells[0].length && cells[row][column] == Cell.E;
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        for (Cell[] cellArray : cells) {
            for (Cell cellValue : cellArray) {
                builder.append(SYMBOLS.get(cellValue)).append(' ');
            }
            builder.append('\n');
        }
        return builder.toString();
    }

}
