package mnkGame;

import java.util.Arrays;

public class UnrealBoard extends MnkBoard {
    public UnrealBoard(int m, int n, int k) {
        super(m, n, k);
    }
    public UnrealBoard(Cell[][] cells) {
        super(1, 1, 1);
        super.cells = new Cell[cells.length][cells[0].length];
        for (int i = 0; i < cells.length; i++) {
            super.cells[i] = Arrays.copyOf(cells[i], cells[i].length);
        }
    }
}
