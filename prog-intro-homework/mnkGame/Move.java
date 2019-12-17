package mnkGame;

public class Move {

    private int row;
    private int column;
    private Cell valueInCell;


    public Move(int row, int column, Cell valueInCell) {
        this.row = row;
        this.column = column;
        this.valueInCell = valueInCell;
    }

    public int getRow() {
        return row;
    }

    public int getColumn() {
        return column;
    }

    public Cell getValueInCell() {
        return valueInCell;
    }

    @Override
    public String toString() {
        return "Move{" +
                "row = " + row +
                ", column = " + column +
                ", valueInCell = " + valueInCell +
                '}';
    }
}
