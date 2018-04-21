using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Text;
using System.Text.RegularExpressions;

namespace NLPCourse.DataTask {

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
            return new String('=', count);
        }

    }
}
