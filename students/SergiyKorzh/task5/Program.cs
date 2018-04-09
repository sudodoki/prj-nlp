using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Xml;
using Korzh.TextUtils;

namespace NLPCourse.Task5
{
    class Program
    {
        static void Main(string[] args)
        {
            Console.OutputEncoding = Encoding.UTF8;

            var feedbackDepot = new FeedbackDepot();

            //--- uncomment next 3 lines if we need to re-scrape all feedbacks in "Monitors" section on Rozetka.ua ---
            //var rozetkaStartUrl = "https://hard.rozetka.com.ua/ua/monitors/c80089/";
            //Console.WriteLine($"Getting data from Rozetka (monitors)...");
            //var rozetkaScraper = new RozetkaScraper();
            //var feedbacks = rozetkaScraper.ScrapeFeedbacks(rozetkaStartUrl);
            //feedbackStorage.SaveToXmlFile("feedbacks.xml", feedbacks);

            var feedbacks = feedbackDepot.LoadFromXmlFile("feedbacks.xml");
            Console.WriteLine($"{feedbacks.Count} feedbacks total");

            //detect language
            feedbackDepot.DetectLang(feedbacks);

            //filtering only Ukrainian feedbacks
            var feedbacksUK = feedbacks.Where(fb => fb.Lang == "ukr").ToList();
            Console.WriteLine($"{feedbacksUK.Count} Ukrainian feedbacks");


            //splitting the list on the training and test sets
            var testList = feedbackDepot.SplitList(feedbacksUK, 0.2);

            Console.WriteLine();

            Console.WriteLine($"Training model ({feedbacksUK.Count} items)...");
            var bc = new BayessClassifier(3);
            bc.TrainReset();

            foreach (var feedback in feedbacksUK) {
                var positive = (double)(feedback.Rating > 3 ? feedback.Rating - 3 : 0);
                var negative = (double)(feedback.Rating < 3 ? 3 - feedback.Rating: 0);
                var neutral = (double)(feedback.Rating == 3 ? 1: 0);
                var tokens = Tokenize(feedback.Text);
                bc.TrainText(tokens, positive, negative, neutral);
            }


            Console.WriteLine($"Checking on the testing set ({testList.Count} items)...");

            //metrics
            double TP = 0, FP = 0, FN = 0, TN = 0;

            foreach (var feedback in testList) {
                var tokens = Tokenize(feedback.Text);
                var predictions = bc.Predict(tokens);

                var maxValue = predictions.Max();

                bool isPositiveOrNeutral = predictions[1] != maxValue;
                bool isNegative = maxValue == predictions[1];

                if (feedback.Rating > 2 && isPositiveOrNeutral) {
                    TP += 1;
                }
                else if (feedback.Rating <= 2 && isPositiveOrNeutral) {
                    FP += 1;
                }
                else if (feedback.Rating <= 2 && !isPositiveOrNeutral) {
                    TN += 1;
                }
                else {
                    FN += 1;
                }
            }

            double precision = TP / (TP + FP);
            double recall = TP / (TP + FN);

            Console.WriteLine($"TP: {TP}; FP: {FP}; TN: {TN}; FN: {FN}");

            Console.WriteLine($"Precision: {precision}; Recall: {recall}");

            Console.ReadKey();
        }

        //very-very-very primitive tokenizer :)
        private static string[] Tokenize(string text) {
            return text.Split(' ', '.', ',', ';', ':', '"', '/', '[', ']');
        }
    }
}
