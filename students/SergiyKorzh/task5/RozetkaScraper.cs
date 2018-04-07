using HtmlAgilityPack;
using System;
using System.Collections.Generic;
using System.Text;
using System.Linq;

using System.IO;

using Korzh.TextUtils;

using SharpNL.Tokenize;
using SharpNL.POSTag;
using System.Text.RegularExpressions;
using SharpNL.NameFind;
using SharpNL.Analyzer;

namespace NLPCourse.Task5 {

    public class RozetkaScraper
    {
        TokenizerModel _tokModel;
        POSModel _posModel;
        TokenNameFinderModel _nameModel;

        AggregateAnalyzer _analyzer;

        public RozetkaScraper() {
            LoadModels();
        }

        public void LoadModels() {
            _analyzer = new AggregateAnalyzer {
                @"models\en-sent.bin",
                @"models\en-token.bin",
                @"models\en-pos-maxent.bin"
            };
        }


        public IList<Feedback> ScrapeFeedbacks(string startUrl) {
            var result = new List<Feedback>();

            var web = new HtmlWeb();
            var page = web.Load(startUrl);

            //read the content of "Career" section of the article
            var careerNode = page.DocumentNode.SelectSingleNode("//*/h2[starts-with(.,'Career')]");
            if (careerNode != null) {
                string text = "";
                var node = careerNode.NextSibling;
                while (node != null && node.Name != "h2") {
                    text += node.InnerText;
                    node = node.NextSibling;
                }

                var doc = new SharpNL.Document("en", text);

                _analyzer.Analyze(doc);                

                foreach (var sentence in doc.Sentences) {
                }
            }

            return result;
        }

    }
}
