using System;
using System.Linq;

using Korzh.TextUtils;

namespace NLPCourse.Task5
{
    class Program
    {
        static void Main(string[] args)
        {
            var rozetkaStartUrl = "https://hard.rozetka.com.ua/ua/monitors/c80089/";

            Console.WriteLine($"Getting data from Rozetka...");

            var rozetkaScraper = new RozetkaScraper();
            var feedbacks = rozetkaScraper.ScrapeFeedbacks(rozetkaStartUrl);

            Console.WriteLine($"{feedbacks.Count} feedbacks found");
            foreach (var fb in feedbacks) {
                Console.WriteLine($" * {fb.Text.FirstNChars(30)}, {fb.SentimentOrigin}");
            }

            Console.WriteLine();

            Console.WriteLine($"Processing results...");


            //metrics
            //double TP = 0, FP = 0, FN = 0, TN = 0;

            //foreach (var item in cast) {
            //    if (item.IsPredicted && item.PredictedScore > 0) {
            //        TP += item.PredictedScore;
            //    }
            //    else if (item.IsPredicted && item.PredictedScore == 0) {
            //        FP += 1;
            //    }
            //    else {
            //        FN += 1;
            //    }
            //}

            //double precision = TP / (TP + FP);
            //double recall = TP / (TP + FN);

            //Console.WriteLine($"{titlesFound} titles from Wikipedia match totally");
            //Console.WriteLine($"{titlesFoundPartially} titles from Wikipedia match partially");

            //Console.WriteLine($"Precision: {precision}; Recall: {recall}");

            //Console.ReadKey();
        }
    }
}
