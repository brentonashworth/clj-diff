package clj_diff;

/**
 * Fast string operations for clj-diff.
 */
public class FastStringOps {

    /**
     * @return the number of common prefix characters for strings a and b
     */
    public static int commonPrefix(String a, String b) {

        int n = Math.min(a.length(), b.length());
        for(int i=0; i<n; i++) {
            if(a.charAt(i) != b.charAt(i)) {
                return i;
            }
        }
        return n;
    }

    /**
     * @return the number of common suffix characters form string a and b
     */
    public static int commonSuffix(String a, String b) {

        int la = a.length();
        int lb = b.length();
        int n = Math.min(la, lb);
        for(int i=1; i<=n; i++) {
            if(a.charAt(la - i) != b.charAt(lb - i)) {
                return i - 1;
            }
        }
        return n;
    }
}
