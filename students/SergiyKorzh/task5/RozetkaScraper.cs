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

    public class RozetkaScraper {
        TokenizerModel _tokModel;
        POSModel _posModel;
        TokenNameFinderModel _nameModel;

        AggregateAnalyzer _analyzer;

        public RozetkaScraper() {
            LoadModels();
        }

        public void LoadModels() {
            //_analyzer = new AggregateAnalyzer {
            //    @"models\en-sent.bin",
            //    @"models\en-token.bin",
            //    @"models\en-pos-maxent.bin"
            //};
        }


        public IList<Feedback> ScrapeFeedbacks(string productsPageUrl) {
            var result = new List<Feedback>();

            var web = new HtmlWeb();
            int index = 1;
            int pageNum = 1;
            var page = web.Load(productsPageUrl);
            while (page != null) {
                Console.WriteLine($"Scanning page {pageNum}...");
                //select all product titles
                var productNodes = page.DocumentNode.SelectNodes("//div[contains(@class, 'g-i-tile-catalog')]");
                if (productNodes != null) {
                    foreach (var node in productNodes) {
                        var titleLinkNode = node.SelectSingleNode(".//div[contains(@class, 'g-i-tile-i-title')]/a");
                        if (titleLinkNode == null) {
                            continue;
                        }

                        var productLink = titleLinkNode.GetAttributeValue("href", "");
                        var productTitle = titleLinkNode.InnerText.Trim();

                        var feedbacksLinkNode = node.SelectSingleNode(".//a[contains(@class, 'g-rating-reviews-link')]");
                        if (feedbacksLinkNode != null) {
                            var feedbacksUrl = feedbacksLinkNode.GetAttributeValue("href", "");

                            ExtractFeedbacks(feedbacksUrl, result);
                            //Console.WriteLine($"{index}: {productTitle}\n {feedbacksUrl}");
                        }

                        index++;
                    }
                }

                //trying to read the next products page
                System.Threading.Thread.Sleep(1000);
                pageNum++;
                var nextPageUrl = productsPageUrl.CombineVia("/", $"page={pageNum}/");
                page = web.Load(nextPageUrl);
                var activePageLinkNode = page.DocumentNode.SelectSingleNode($"//li[@id='page{pageNum}']");
                if (activePageLinkNode == null || !activePageLinkNode.GetAttributeValue("class", "").Contains("active")) {
                    break;
                }
            }
            return result;
        }

        private void ExtractFeedbacks(string commentsUrl, List<Feedback> feedbacks) {
            var web = new HtmlWeb();
            //commentsUrl = "https://hard.rozetka.com.ua/ua/lg_27mp59g_p_aruz/p16940522/comments/";
            var page = web.Load(commentsUrl);
            int pageNum = 1;
            while (page != null) {
                Console.WriteLine($"Scanning comments page {pageNum}...");
                var feedbackNodes = page.DocumentNode.SelectNodes("//div[@id='comments']/article");
                if (feedbackNodes == null) {
                    break;
                }

                foreach (var node in feedbackNodes) {
                    var ratingNode = node.SelectSingleNode(".//meta[@itemprop='ratingValue']"); //div[itemprop='reviewRating']/
                    if (ratingNode == null) {
                        continue;
                    }

                    int rating = ratingNode.GetAttributeValue("content", 0);

                    var textNode = node.SelectSingleNode(".//div[@class='pp-review-text']");
                    if (textNode == null) {
                        continue;
                    }

                    var text = textNode.InnerText.Trim();
                    var feedback = new Feedback {
                        Rating = rating,
                        Text = text
                    };

                    feedbacks.Add(feedback);
                }

                //trying to read the next comments page
                pageNum++;
                var nextPageUrl = commentsUrl.CombineVia("/", $"page={pageNum}/");
                page = web.Load(nextPageUrl);
                var activePageLinkNode = page.DocumentNode.SelectSingleNode($"//li[@id='page {pageNum}']");
                if (activePageLinkNode == null || !activePageLinkNode.GetAttributeValue("class", "").Contains("active")) {
                    break;
                }
            }
        }
    }
}
