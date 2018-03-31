using System;
using System.Xml;
using System.Linq;
using System.Collections.Generic;
using System.IO;
using System.Text.RegularExpressions;

namespace data01
{
    class Program {

        private static int totalWords = 0;
        private static int wordsWithSynonims = 0;


        static void Main(string[] args) {
            string filePath = "itwiktionary-20180301-pages-meta-current.xml";

            Console.WriteLine("Processing Wiktionary dump...");
            ReadWiktionaryDump(filePath);

            Console.WriteLine($"{totalWords} words processed");
            Console.WriteLine($"{wordsWithSynonims} of them has synonyms");
#if DEBUG 
            Console.ReadKey();
#endif
        }


        private static void ReadWiktionaryDump(string xmlFilePath) {
            if (!File.Exists(xmlFilePath)) {
                Console.WriteLine("No file: " + xmlFilePath);
                return;
            }
            using (var fileReader = new StreamReader(xmlFilePath)) {
                XmlReader reader = XmlReader.Create(fileReader, new XmlReaderSettings { ConformanceLevel = ConformanceLevel.Fragment });
                while (reader.Read()) {
                    if (reader.NodeType == XmlNodeType.Element) {
                        if (reader.LocalName == "page") {
                            ReadPage(reader);
                        }
                    }
                }
            }
        }

        private static void ReadPage(XmlReader reader) {
            string word = null;
            int ns = -99;
            while (reader.Read()) {

                if (reader.NodeType == XmlNodeType.Element) {
                    if (reader.LocalName == "title") {
                        word = reader.ReadElementContentAsString();
                    }
                    else if (reader.LocalName == "ns") {
                        ns = reader.ReadElementContentAsInt();
                    }
                    else if (reader.LocalName == "revision" && ns == 0) {
                        ReadRevision(reader, word);
                    }
                    else {
                        reader.Skip();
                    }
                }
                else if (reader.NodeType == XmlNodeType.EndElement) {
                    break;
                }
            }
        }

        private static Regex oneSynonymRegex = new Regex(@"\[\[(.+?)\]\]");

        private static void ReadRevision(XmlReader reader, string word) {
            totalWords++;
            while (reader.Read()) {
                if (reader.NodeType == XmlNodeType.Element) {
                    if (reader.LocalName == "text") {
                        var synonyms = new HashSet<string>();
                        var textReader = new StringReader(reader.ReadElementContentAsString());
                        string line;
                        bool isInsideSynBlock = false;
                        while ((line = textReader.ReadLine()) != null) {
                            if (isInsideSynBlock) {
                                if (line.Trim().StartsWith("{{-")) {
                                    break;
                                }

                                var matches = oneSynonymRegex.Matches(line);
                                foreach (Match match in matches) {
                                    synonyms.Add(match.Groups[1].Value);
                                }
                            }
                            else if (line.StartsWith("{{-sin-}}")) {
                                isInsideSynBlock = true;
                            }
                        }

                        if (synonyms.Count > 0) {
                            wordsWithSynonims++;
                            Console.WriteLine(word + ": " + string.Join(", ", synonyms));
                        }
                    }
                    else {
                        reader.Skip();
                    }
                }
                else if (reader.NodeType == XmlNodeType.EndElement) {
                    break;
                }
            }
        }


    }
}
