package expression;

public class Main {
    public static void main(String[] args) {
        System.out.println(new Subtract(
                new Multiply(
                        new Const(2),
                        new Variable("x")
                ),
                new Const(3)
        ).toMiniString());
    }
}
