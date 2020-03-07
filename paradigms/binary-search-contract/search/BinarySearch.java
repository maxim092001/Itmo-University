package search;

public class BinarySearch {



    // Pre: forall i = 0..args.length - 1: args[i] == value  && forall i = 1..args.length - 2: args[i] >= args[i + 1] &&
    // args.length > 0
    // Post: Res = i : i + 1 == args.length || (args[i] <= args[0] && i - min)
    public static void main(String[] args) {
        int key = Integer.parseInt(args[0]);
        int[] numbers = new int[args.length - 1];

        for (int i = 1; i < args.length; i++) {
            numbers[i - 1] = Integer.parseInt(args[i]);
        }
        System.out.println(iterativeBinarySearch(key, numbers));
    }


    // Pre: forall i, j = 0..numbers.length - 1 : i < j -> numbers[i] >= number[j]
    // Post: Res = (index : (index >= 0 && index < numbers.length && numbers[index] <= key && (index == 0 ||
    // numbers[index - 1] > key)) || (index == numbers.length && numbers[numbers.length - 1] > index)
    public static int iterativeBinarySearch(int key, int[] numbers) {
        // Pre: true
        // Post: Res = (left = -1)
        int left = -1;
        // Pre: true
        // Post: Res = (right = numbers.length)
        int right = numbers.length;


        // Inv: ((right'  - left' + 1) * 2 <= (right - left + 1)) && left + 1 < right
        // && (left == -1 || numbers[left] > key) && (right == numbers.length || numbers[right] <= key)
        while (left + 1 != right) {

            // Pre:  true
            // Post: Res = (mid = (right + left) / 2)
            int mid = (right + left) / 2;

            if (numbers[mid] <= key) {
                // Pre: numbers[mid] <= key
                right = mid;
                // Post: Res = (left' = left, right' = mid)
                // numbers[right] <= key && ((right' - mid + 1) * 2 <= (right - left + 1) -> (right' - left' + 1) * 2
                // <= (right - left + 1))
            } else {
                // Pre: numbers[mid] > key
                left = mid;
                // Post: Res = (left' = mid, right' = right)
                // numbers[left] > key && ((mid - left' + 1) * 2 <= (right - lefft + 1) -> (right' - left' + 1) * 2
                // <= (right - left + 1))
            }
        }

        // Post: Res = (right : left == right - 1 && (right >= 0 && right < numbers.length && numbers[right] <= key
        // && (right == 0 || numbers[right - 1] > key)) || (right == numbers.length
        // && numbers[numbers.length - 1] > right))
        return right;
    }

    // Pre: forall i, j = 0..numbers.length - 1 : i < j -> numbers[i] >= number[j]
    // Post: Res = (index : (index >= 0 && index < numbers.length && numbers[index] <= key && (index == 0 ||
    // numbers[v - 1] > key)) || (index == numbers.length && numbers[numbers.length - 1] > index)
    public static int recursiveBinarySearch(int key, int[] numbers) {
        return recursiveBinarySearch(key, numbers, -1, numbers.length);
    }


    // Pre: forall i, j = 0..numbers.length - 1 : i < j -> numbers[i] >= number[j]
    // Post: Res = (index : (index >= 0 && index < numbers.length && numbers[index] <= key && (index == 0 ||
    // numbers[v - 1] > key)) || (index == numbers.length && numbers[numbers.length - 1] > index)
    // Inv: ((right'  - left' + 1) * 2 <= (right - left + 1)) && left + 1 < right
    // && (left == -1 || numbers[left] > key) && (right == numbers.length || numbers[right] <= key)
    private static int recursiveBinarySearch(int key, int[] numbers, int left, int right) {
        if (left == right - 1) {
            // left == right - 1 && (right >= 0 && right < numbers.length && numbers[right] <= key && (right == 0 ||
            // numbers[right - 1] > key)) || (right == numbers.length && numbers[numbers.length - 1] > right)
            return right;
        }

        // Pre: true
        // Post: mid > left && right < mid && (right - mid +  1) * 2 <= (right - left + 1) && (mid - left + 1) * 2
        // <= (right - left + 1)
        int mid = (right + left) / 2;

        if (numbers[mid] > key) {
            // Pre: numbers[mid] > key
            // Post: numbers[mid] > key && left' = mid && numbers[left'] > key && (mid - left'  + 1) * 2
            // <= (right - left + 1)
            return recursiveBinarySearch(key, numbers, mid, right);
        } else {
            // Pre: numbers[mid] <= key
            // Post: numbers[mid] <= key && right' =  mid && numbers[right'] <= key && (right' - mid + 1) * 2
            // <= (right - left + 1)
            return recursiveBinarySearch(key, numbers, left, mid);
        }
    }
}
