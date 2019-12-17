public class SumDouble {
    public static void main(String[] args) {
        double sum = 0;

        for (String s : args) {

            int start = 0;
            int end = 0;
            while (start < s.length()) {

                while (start < s.length() && Character.isWhitespace(s.charAt(start))) {
                    start++;
                }

                end = start;

                while (end < s.length() && !Character.isWhitespace(s.charAt(end))) {
                    end++;
                }

                if (start < end) {
                    sum = Double.sum(sum, Double.parseDouble(s.substring(start, end)));
                }

                start = end;
            }
        }
        System.out.print(sum);

    }

}
