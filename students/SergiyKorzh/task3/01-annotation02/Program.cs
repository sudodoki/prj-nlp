using System;
using System.Xml;
using System.Linq;
using System.Collections.Generic;
using System.IO;


namespace data01
{
    class Program
    {
        private static Dictionary<string, IList<Mistake>> _teachers = new Dictionary<string, IList<Mistake>>();

        static void Main(string[] args)
        {
            Console.WriteLine("Reading SQML 1...");
            ReadSqml("official-2014.0.sgml");

            Console.WriteLine("Reading SQML 2...");
            ReadSqml("official-2014.1.sgml");

            Console.WriteLine("Comparing annotations...");
            int harmony = 0;
            int total = 0;
            
            var teacherIds = _teachers.Keys.ToArray();
            var ftid = teacherIds[0];
            var firstTeacherMistakes = _teachers[ftid];
            //scan all annotations of the first teacher compare them with the mistakes fixed by others
            foreach (var mistake in firstTeacherMistakes) {
                bool othersHaveSameMistake = true;
                for (int i = 1; i < teacherIds.Length; i++) {
                    var mistake2 = FindMistake(teacherIds[i], mistake);
                    if (mistake2 != null) {
                        mistake2.InHarmony = true; //not quite correct but will work for 2 teachers
                        harmony++;
                    }
                    else {
                        othersHaveSameMistake = false;
                        break;
                    }
                }
                if (othersHaveSameMistake) {
                    mistake.InHarmony = true;
                }
            }

            foreach (var entry in _teachers) {
                total += entry.Value.Count;
            }
            
            Console.WriteLine($"Total annotations (made by {_teachers.Count} teachers): {total}");
            Console.WriteLine($"Agreements: {harmony}");

            Console.ReadKey();
        }


        private static Mistake FindMistake(string teacherId, Mistake mistakeToCheck) {
            foreach (var mistake in _teachers[teacherId]) {
                if (mistake.Equals(mistakeToCheck)) {
                    return mistake;
                }
            }
            return null;
        }

        private static void ReadSqml(string sgmlFilePath) {
            if (!File.Exists(sgmlFilePath)) {
                Console.WriteLine("No file: " + sgmlFilePath);
                return;
            }
            using (var fileReader = new StreamReader(sgmlFilePath))  {
                XmlReader reader = XmlReader.Create(fileReader, new XmlReaderSettings { ConformanceLevel = ConformanceLevel.Fragment });
                while (reader.Read()) {
                    if (reader.NodeType == XmlNodeType.Element) {
                        if (reader.LocalName == "ANNOTATION") {
                            ReadAnnotation(reader);
                        }
                    }
                }
            }
        }

        private static void ReadAnnotation(XmlReader reader) {
            var teacherId = reader.GetAttribute("teacher_id");
            if (teacherId != null) {
                while (reader.Read()) {
                    if (reader.NodeType == XmlNodeType.Element && reader.LocalName == "MISTAKE") {
                        var mistake = ReadMistake(reader);
                        if (!_teachers.TryGetValue(teacherId, out var mistakes)) {
                            mistakes = new List<Mistake>();
                            _teachers[teacherId] = mistakes;
                        }
                        mistakes.Add(mistake);
                    }
                    else if (reader.NodeType == XmlNodeType.EndElement) {
                        break;
                    }
                }
            }
        }

        private static Mistake ReadMistake(XmlReader reader) {
            var mistake = new Mistake();
            mistake.Span.Item1 = int.Parse(reader.GetAttribute("start_par"));
            mistake.Span.Item2 = int.Parse(reader.GetAttribute("start_off"));
            mistake.Span.Item3 = int.Parse(reader.GetAttribute("end_par"));
            mistake.Span.Item4 = int.Parse(reader.GetAttribute("end_off"));

            while (reader.Read()) {
                if (reader.NodeType == XmlNodeType.Element) {
                    if (reader.LocalName == "TYPE") {
                        mistake.Type = reader.ReadElementContentAsString();
                    }
                    else if (reader.LocalName == "CORRECTION") {
                        mistake.Correction = reader.ReadElementContentAsString();
                    }
                }
                else if (reader.NodeType == XmlNodeType.EndElement) {
                    break;
                }
            }

            return mistake;
        }


        private void ReadOpenCrawlIndex() {
            XmlReaderSettings xmlSettings = new XmlReaderSettings {
                DtdProcessing = DtdProcessing.Ignore,
                ConformanceLevel = ConformanceLevel.Fragment,
                ValidationFlags = System.Xml.Schema.XmlSchemaValidationFlags.None,
                ValidationType = ValidationType.None
            };

            XmlReader reader = XmlReader.Create("https://commoncrawl.s3.amazonaws.com/", xmlSettings);
            long minSize = long.MaxValue;
            string minPath = "", path = "";
            while (reader.Read()) {
                if (reader.NodeType == XmlNodeType.Element) {

                    if (reader.LocalName == "Key") {
                        path = reader.ReadElementContentAsString();
                    }
                    else if (reader.LocalName == "Size") {
                        string sizeStr = reader.ReadElementContentAsString();
                        if (long.TryParse(sizeStr, out long size)) {
                            if (size < minSize) {
                                minSize = size;
                                minPath = path;
                            }
                        }
                    }
                }
            }
            Console.WriteLine("Min key: " + minPath);
            Console.WriteLine("Size: " + minSize);
        }


    }
    public class Mistake : IEquatable<Mistake> {
        public string Type;
        public ValueTuple<int, int, int, int> Span;
        public string Correction;

        public bool InHarmony;

        public bool Equals(Mistake other) {
            return this.Type == other.Type
                && this.Span.Item1 == other.Span.Item1
                && this.Span.Item2 == other.Span.Item2
                && this.Span.Item3 == other.Span.Item3
                && this.Span.Item4 == other.Span.Item4
                && this.Correction == other.Correction;
        }

    }

}
