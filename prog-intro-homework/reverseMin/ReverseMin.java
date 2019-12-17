import java.io.IOException;
import java.util.Arrays;

class ReverseChecker implements Checker {
    public boolean isWordCharacter(char c) {
        return !Character.isWhitespace(c);
    }
}

public class ReverseMin {

    private static final int MAX_SIZE = 10;

    private static ReverseChecker reverseChecker = new ReverseChecker();

    public static void main(String[] args) throws IOException {


        int[] numbers = new int[MAX_SIZE];
        int[] temp = new int[MAX_SIZE];

        int[] columnSum = new int[MAX_SIZE];
        int[] stringSum = new int[MAX_SIZE];

        int index = 0;
        try (Scanner text = new Scanner(System.in, reverseChecker)) {

            while (!text.isEmpty()) {
                int sizeNums = 0;

                while (!text.isEndOfLine()) {
                    int nxt = text.nextInt();

                    columnSum[sizeNums] = Math.min(columnSum[sizeNums] == 0 ? Integer.MAX_VALUE : columnSum[sizeNums], nxt);
                    temp[sizeNums++] = nxt;
                    stringSum[index] = Math.min(stringSum[index] == 0 ? Integer.MAX_VALUE : stringSum[index], nxt);

                    if (sizeNums >= columnSum.length || sizeNums >= temp.length) {
                        columnSum = expandArraySize(columnSum);
                        temp = expandArraySize(temp);
                    }
                }
                text.skipAllLine();

                numbers[index] = sizeNums;


                index++;

                if (index >= numbers.length || index >= stringSum.length) {
                    stringSum = expandArraySize(stringSum);
                    numbers = Arrays.copyOf(numbers, numbers.length * 2);
                }
            }

            text.close();
            for (int i = 0; i < index; i++) {
                for (int j = 0; j < numbers[i]; j++) {
                    System.out.print(Math.min(columnSum[j], stringSum[i]) + " ");
                }
                System.out.println();
            }
        }
    }

    private static int[] expandArraySize(int[] arr) {
        return Arrays.copyOf(arr, arr.length * 2);
    }
}
