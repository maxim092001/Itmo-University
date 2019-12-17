package mnkGame;

public interface Board {
    Result makeMove(Move move);
    Position getPosition();
    Cell getCell();
    void fillBoard(int m, int n, int k);
    void clear();
}
