package likely;

public class DamerauLevenshtein {

        private static int minimum(int a, int b, int c) {
                return Math.min(Math.min(a, b), c);
        }
 
    /*        public static int distanceOf(CharSequence str1,
                        CharSequence str2) {
                int[][] distance = new int[str1.length() + 1][str2.length() + 1];
		
 
                for (int i = 0; i <= str1.length(); i++)
                        distance[i][0] = i;
                for (int j = 1; j <= str2.length(); j++)
                        distance[0][j] = j;
 
                for (int i = 1; i <= str1.length(); i++) {
                        for (int j = 1; j <= str2.length(); j++) {
                                distance[i][j] = minimum(
                                                distance[i - 1][j] + 1,
                                                distance[i][j - 1] + 1,
                                                distance[i - 1][j - 1]
                                                                + ((str1.charAt(i - 1) == str2.charAt(j - 1)) ? 0
                                                                                : 1));

				if ((i > 1) && (j > 1) && (str1.charAt(i-1) == str2.charAt(j-2)) && (str1.charAt(i-2) == str2.charAt(j-1))) {
					distance[i][j] = Math.min(distance[i][j], distance[i-2][j-2]+1);
				}
			}
		}
                return distance[str1.length()][str2.length()];
        }
    */



    public static int distanceOf(CharSequence str1, CharSequence str2, int maxDistance) {
        int len1 = str1.length();
        int len2 = str2.length();

        // Early bail: impossible to reach within maxDistance
        if (Math.abs(len1 - len2) > maxDistance) {
            return -1;
        }

        int[][] distance = new int[len1 + 1][len2 + 1];

        for (int i = 0; i <= len1; i++) distance[i][0] = i;
        for (int j = 1; j <= len2; j++) distance[0][j] = j;

        for (int i = 1; i <= len1; i++) {
            int minInRow = Integer.MAX_VALUE;

            for (int j = 1; j <= len2; j++) {
                int cost = (str1.charAt(i - 1) == str2.charAt(j - 1)) ? 0 : 1;

                int del = distance[i - 1][j] + 1;
                int ins = distance[i][j - 1] + 1;
                int sub = distance[i - 1][j - 1] + cost;

                int value = minimum(del, ins, sub);

                if (i > 1 && j > 1
                        && str1.charAt(i - 1) == str2.charAt(j - 2)
                        && str1.charAt(i - 2) == str2.charAt(j - 1)) {
                    value = Math.min(value, distance[i - 2][j - 2] + 1);
                }

                distance[i][j] = value;
                if (value < minInRow) minInRow = value;
            }

            // Early abort: row minimum already too large
            if (minInRow > maxDistance) return -1;
        }

        int result = distance[len1][len2];
        return result <= maxDistance ? result : -1;
    }


}
