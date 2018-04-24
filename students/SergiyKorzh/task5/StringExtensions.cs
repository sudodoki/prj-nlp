using System;


namespace Korzh.TextUtils {

    public static class StringExtensions
    {
       
        /// <summary>
        /// Splits the full name to first and last name.
        /// </summary>
        /// <param name="name">The full name.</param>
        /// <returns>System.String.</returns>
        /// <exception cref="Exception">param name in SplitName cannot be null</exception>
        public static (string firstName, string lastName ) SplitName(this string name) {
            if (name != null){
                string[] nameParts = name.Split(new char[] { ' ' }, StringSplitOptions.RemoveEmptyEntries);
                if (nameParts.Length > 1) {
                    return (nameParts[0], nameParts[1]);
                } else {
                    return (nameParts[0], "");
                }
            } else {
                throw new ArgumentNullException(nameof(name));
            }
        }

        public static string CombineVia(this string baseUri, string separator, string uri) {
            if (baseUri == null) {
                throw new ArgumentNullException(nameof(baseUri));
            }

            if (uri == null) {
                throw new ArgumentNullException(nameof(uri));
            }

            var b1 = baseUri.EndsWith(separator, StringComparison.InvariantCultureIgnoreCase);
            var b2 = uri.StartsWith(separator, StringComparison.InvariantCultureIgnoreCase);

            if ((!b1 && b2) || (b1 && !b2)) {
                return baseUri + uri;
            }
            else if (!b1 && !b2) {
                return baseUri + separator + uri;
            }
            else {
                return baseUri.Remove(baseUri.Length - separator.Length) + uri;
            }
        }

        public static ValueTuple<string, string> SplitByLastEntry(this string s, char ch) {
            if (s == null) {
                return (null, null);
            }

            int p = s.LastIndexOf(ch);
            if (p >= 0) {
                string firstPart = p > 0 ? s.Substring(0, p) : "";
                string lastPart = p < s.Length - 1 ? s.Substring(p + 1) : "";
                return (firstPart, lastPart);
            }
            else {
                return ("", s);
            }
        }

        public static string Replicate(this char ch, int count) {
            return new String(ch, count);
        }

        public static bool IsCapitalized(this string s) {
            if (string.IsNullOrEmpty(s)) {
                return false;
            }

            var firstChar = s.Substring(0, 1);

            return firstChar == firstChar.ToUpperInvariant();
        }

        public static bool IsNamePosTag(this string posTag) { 
            if (string.IsNullOrEmpty(posTag)) {
                return false;
            }

            return posTag == "NNP" || posTag == "NNPS";
        }

        public static bool IsPunctuation(this string ch) {
            if (string.IsNullOrEmpty(ch)) {
                return false;
            }

            return ch == "." || ch == "," || ch == "!" || ch == "?" || ch == ";" || ch == ":";
        }


        public static string AddWord(this string s, string word) {
            if (!string.IsNullOrEmpty(s)) {
                return s + " " + word;
            }
            else {
                return word;
            }
        }

        public static string FirstNChars(this string s, int len) {
            if (s == null) {
                return null;
            }

            if (s.Length < len) {
                return s;
            }
            else {
                return s.Substring(0, len);
            }

        }

    }
}
