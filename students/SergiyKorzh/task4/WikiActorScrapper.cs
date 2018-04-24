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

namespace NLPCourse.Task4
{
    public class WikiActorScrapper
    {
        TokenizerModel _tokModel;
        POSModel _posModel;
        TokenNameFinderModel _nameModel;

        AggregateAnalyzer _analyzer;

        public WikiActorScrapper() {
            LoadModels();
        }

        public void LoadModels() {
            _analyzer = new AggregateAnalyzer {
                @"models\en-sent.bin",
                @"models\en-token.bin",
                @"models\en-pos-maxent.bin"
            };
        }


        public IList<string> ScrapeMovieTitles(string actorWikipediaUrl, string[] ignoreWords) {
            var result = new List<string>();

            var web = new HtmlWeb();
            var page = web.Load(actorWikipediaUrl);

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
                    Token prevToken = null;
                    string title = "";
                    bool withinTitle = false; 
                    foreach (var token in sentence.Tokens) {
                        if (!withinTitle && IsTitleStart(token, prevToken)) {
                            withinTitle = true;
                            title = "";
                        }
                        else if (withinTitle) {
                            if (IsTitleEnd(token, prevToken)) {
                                if (prevToken != null && prevToken.POSTag.IsNamePosTag()) {
                                    title = title.AddWord(prevToken.Lexeme);
                                }
                                if (IsMovieTitle(title)) {
                                    result.Add(title);
                                }
                                title = "";
                                withinTitle = false;
                            }
                            else {
                                title = title.AddWord(prevToken.Lexeme);
                            }
                        }
                        prevToken = token;
                    }
                }
            }

            return result;
        }

        private static Regex wordsOnlyRegex = new Regex(@"^\w+$");

        private bool IsTitleStart(Token token, Token prevToken) {
            //if capitalized
            if (!token.Lexeme.IsCapitalized()) {
                return false;
            }

            //has some "nice" token before (NOT
            var hasNiceTokenBefore = prevToken != null && (prevToken.POSTag == "IN" || prevToken.Lexeme == "film");
            if (!hasNiceTokenBefore) {
                return false;
            }

            if (IsName(token)) {
                return false;
            }

            if (IsDate(token)) {
                return false;
            }
            return true;
        }

        private bool IsName(Token token) {
            return false; //check if this token is the name of the person
        }

        private static string[] _months = { "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December" };

        private bool IsDate(Token token) {
            return _months.Contains(token.Lexeme);
        }

        private bool IsTitleEnd(Token token, Token prevToken) {
            if (token.Lexeme.IsPunctuation() && token.Lexeme != ":") {
                return true;
            }
            
            bool isNotName = token.POSTag != "NNP" && token.POSTag != "NNPS";

            bool hasBadTag = token.POSTag == "VBG";

            bool prevNotName = (prevToken != null) && prevToken.POSTag != "NNP" && prevToken.POSTag != "NNPS";

            return isNotName && prevNotName || hasBadTag;
        }

        private bool IsMovieTitle(string name) {
            if (string.IsNullOrEmpty(name)) {
                return false;
            }
            return true;
        }
    }
}
