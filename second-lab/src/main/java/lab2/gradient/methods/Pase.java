package lab2.gradient.methods;

import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

public class Pase {
    public static void main(String[] args) {
        Scanner in = new Scanner(System.in);
        String[] q = in.nextLine().replace(",", "").replace("(", "").replace(")", "").split(" ");
        List<String> x = new ArrayList<>();
        List<String> y = new ArrayList<>();
        int z = 200;
        for (int i = 0; i < q.length; i += 2) {
            System.out.println(q[i] + " " + (Math.max(Integer.parseInt(q[i + 1]) + z, 0)) + "\\\\");
        }
    }
}
