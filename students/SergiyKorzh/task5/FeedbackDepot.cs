using SharpNL.Analyzer;
using System;
using System.Collections.Generic;
using System.IO;
using System.Reflection.Metadata;
using System.Text;
using System.Xml;

namespace NLPCourse.Task5
{
    public class FeedbackDepot
    {

        public IList<Feedback> LoadFromXmlFile(string filePath) {
            var feedbacks = new List<Feedback>();

            var xmlSettings = new XmlReaderSettings {
            };
            using (var reader = XmlTextReader.Create(new FileStream(filePath, FileMode.Open), xmlSettings)) {
                while (reader.Read()) {
                    if (reader.NodeType == XmlNodeType.Element && reader.LocalName == "feedback") {
                        var ratingVal = reader.GetAttribute("rating");
                        var lang = reader.GetAttribute("lang");
                        var feedback = new Feedback {
                            Rating = int.Parse(ratingVal),
                            Lang = lang ?? "",
                            Text = reader.ReadElementContentAsString()
                        };
                        feedbacks.Add(feedback);
                    }
                }
            }
            return feedbacks;
        }

        public void SaveToXmlFile(string filePath, IList<Feedback> feedbacks) {
            var xmlSettings = new XmlWriterSettings {
                Indent = true,
                Encoding = Encoding.UTF8,
                OmitXmlDeclaration = true
            };
            using (var writer = XmlTextWriter.Create(filePath, xmlSettings)) {
                writer.WriteStartElement("feedbacks");
                foreach (var feedback in feedbacks) {
                    writer.WriteStartElement("feedback");
                    writer.WriteAttributeString("rating", feedback.Rating.ToString());
                    writer.WriteAttributeString("lang", feedback.Lang);
                    writer.WriteString(feedback.Text);
                    writer.WriteEndElement();
                }

                writer.WriteEndElement();
                writer.Flush();
            }
        }

        public IList<T> SplitList<T>(IList<T> list, double portion) {
            if (portion < 0 || portion > 1) {
                throw new Exception("portion must be witin [0, 1]");
            }

            var testList = new List<T>();

            int index = Convert.ToInt32(list.Count * (1 - portion));

            while (index < list.Count) {
                testList.Add(list[index]);
                list.RemoveAt(index);
            }

            return testList;
        }

        public void DetectLang(IList<Feedback> feedbacks) {
            var detector = new LanguageDetection.LanguageDetector();
            detector.AddLanguages("eng", "rus", "ukr");
            foreach (var feedback in feedbacks) {
                feedback.Lang = detector.Detect(feedback.Text);
            }
        }
    }
}
