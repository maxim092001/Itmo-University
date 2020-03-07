package search;

public class BinarySearchSpan {


    // Pre: forall i = 0..args.length - 1: args[i] == (int) && forall i = 1..args.length - 2: args[i] >= args[i + 1]
    // && args.length > 0
    // Post: Res = (i, length) : (i + 1 == args.length || (args[i + 1] <= args[0] && args[i] > args[0]))
    // && (forall j : (i + 1) .. (i + length) : args[j] == args[0])
    public static void main(String[] args) {
        int key = Integer.parseInt(args[0]);
        int[] numbers = new int[args.length - 1];

        for (int i = 1; i < args.length; i++) {
            numbers[i - 1] = Integer.parseInt(args[i]);
        }

        int left = recursiveBinarySearch(key, numbers, -1, numbers.length, false);
        int length = iterativeBinarySearch(key, numbers, true) - left;
        System.out.println(left + " " + length);
    }


    // Pre: forall i, j = 0..numbers.length - 1 : i < j -> numbers[i] >= number[j]
    // Post: (rightEntry == false && Res >= 0  && Res < numbers.length && (numbers[Res + 1] > key) && (right == 0 || numbers[right - 1] > key))
    // || (rightEntry == true && right >= 0 && right < numbers.length && (right : numbers[right - 1] >= key) && (right == 0 || numbers[right - 1] >= key))
    // || (right == numbers.length && numbers[numbers.length - 1] > right))
    public static int iterativeBinarySearch(int key, int[] numbers, boolean rightEntry) {
        int left = -1;
        // (left = -1)
        int right = numbers.length;
        // (right = numbers.length)

        // Pre: left != right - 1

        // Post: left == right - 1 && (rightEntry == false && right >= 0 && right < numbers.length && (right : numbers[right + 1] > key) && (right == 0 || numbers[right - 1] >= key))
        // || (rightEntry == true && right >= 0 && right < numbers.length && (right : numbers[right - 1] >= key) && (right == 0 || numbers[right - 1] >= key)) || (right == numbers.length &&
        // numbers[numbers.length - 1] > right)

        // Inv: ((right' - left' + 1) * 2 <= (right - left + 1)) && right - 1 > left && (left == -1 || (rightEntry == false && numbers[left] > key) || (rightEntry = true && numbers[left] >= key))
        // && (right == numbers.length || (rightEntry == false && numbers[right] <= key)
        // || (rightEntry == true && numbers[right] < key))
        while (left != right - 1) {

            int mid = (right + left) / 2;
            // mid > left && mid < right && (right - mid + 1) * 2 <= (right - left + 1)
            // && (mid - left + 1) * 2 <= (right - left + 1)
            if (numbers[mid] > key || (rightEntry && numbers[mid] == key)) {
                left = mid;
                // left' = left, right' = mid
                // numbers[left'] > key || (rightEntry == true && numbers[left'] == key) ||
                // ((mid - left' + 1) * 2 <= (right - left + 1) -> (right' - left' + 1) * 2 <= (right - left + 1)
            } else {
                right = mid;
                // left' = mid, right' = right
                // numbers[right'] <= key || (rightEntry == true && numbers[right'] < key) ||
                // (right' - mid + 1) * 2 <= (right - left + 1) -> (right' - left' + 1) * 2 <= (right - left + 1)
            }
        }

        return right;
    }

    // Pre: forall i, j = left + 1..right - 1 : i < j -> numbers[i] >= number[j] && left < right
    // Post: Res = (right : (rightEntry == false && right >= 0 && right < numbers.length && (right : numbers[right] > key)) && (right == 0 || numbers[right - 1] > key))
    // || (rightEntry == true && right >= 0 && right < numbers.length && (right : numbers[right - 1] >= key))
    // && (right == 0 || numbers[right - 1] >= key)) || (right == numbers.length && numbers[numbers.length - 1] > right) && left == right - 1)

    private static int recursiveBinarySearch(int key, int[] numbers, int left, int right, boolean rightEntry) {
        if (left == right - 1) {
            // left == right - 1 && (rightEntry == false && right >= 0 && right < numbers.length && numbers[right] <= key && (right == 0 || numbers[right  - 1] >= key))
            // || (rightEntry == true && right >= 0 && right < numbers.length && numbers[right] < key && (right == 0 || numbers[right - 1] >= key))
            // || (right == numbers.length && numbers[numbers.length - 1] > right)
            return right;
        }

        // mid > left && right < mid && (right - mid +  1) * 2 <= (right - left + 1) && (mid - left + 1) * 2
        // <= (right - left + 1)
        int mid = (right + left) / 2;

        if (numbers[mid] > key || (rightEntry && numbers[mid] == key)) {
            // Pre: numbers[mid] > key || (rightEntry && numbers[mid] == key)
            // Post: left' = mid, right' = right && numbers[left'] > key || (rightEntry == true && numbers[left'] == key)
            // || ((mid - left' + 1) * 2 <= (right - left + 1) -> (right' - left' + 1) * 2 <= (right - left + 1))
            return recursiveBinarySearch(key, numbers, mid, right, rightEntry);
        }

        // Post: left' = left, right' = right &&
        // numbers[right'] <= key || (rightEntry && numbers[right'] < key)
        // || ((right' - mid + 1) * 2 <= (right - left + 1) -> (right' - left' + 1) * 2 <= (right - left + 1)
        return recursiveBinarySearch(key, numbers, left, mid, rightEntry);
    }
}
