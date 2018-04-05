using System;
using System.Linq;

namespace NLPCourse.Task4
{
    class Program
    {
        static void Main(string[] args)
        {
            var actorName = "Stana Katic";
            var actorTmdbId = "34408";

            Console.WriteLine($"Getting {actorName} credits from TMDB...");
            var imdbReader = new TmdbReader();
            var cast = imdbReader.GetActorCast(actorTmdbId);

            Console.WriteLine($"Total titles {actorName} played in : {cast.Count}");
            foreach (var item in cast) {
                Console.WriteLine($" * {item.FilmTitle}, {item.FilmReleaseDate}, ({item.character})");
            }
            Console.WriteLine();


            Console.WriteLine($"Getting {actorName} titles from Wikipedia");

            var wikiActorScraper = new WikiActorScrapper();
            var titles = wikiActorScraper
                .ScrapeMovieTitles("https://en.wikipedia.org/wiki/Stana_Katic", new string[] { });

            Console.WriteLine($"{titles.Count} titles found");
            foreach (var ttl in titles) {
                Console.WriteLine($" * {ttl}");
            }

            Console.WriteLine();

            Console.WriteLine($"Processing results...");
            int titlesFound = 0, titlesFoundPartially = 0;
            foreach (var ttl in titles) {
                var item = cast.FirstOrDefault(c => c.FilmTitle == ttl);
                if (item != null) {
                    item.IsPredicted = true;
                    item.PredictedScore = 1;
                    titlesFound++;
                }
                else {
                    item = cast.FirstOrDefault(c => c.FilmTitle.Contains(ttl) || ttl.Contains(c.FilmTitle));
                    if (item != null) {
                        item.IsPredicted = true;
                        item.PredictedScore = 0.5;
                        titlesFoundPartially++;
                    }
                    else {
                        cast.Add(new CastItem {
                            IsPredicted = true,
                            title = ttl
                        });
                    }
                }
            }

            //metrics
            double TP = 0, FP = 0, FN = 0, TN = 0;

            foreach (var item in cast) {
                if (item.IsPredicted && item.PredictedScore > 0) {
                    TP += item.PredictedScore;
                }
                else if (item.IsPredicted && item.PredictedScore == 0) {
                    FP += 1;
                }
                else {
                    FN += 1;
                }
            }

            double precision = TP / (TP + FP);
            double recall = TP / (TP + FN);

            Console.WriteLine($"{titlesFound} titles from Wikipedia match totally");
            Console.WriteLine($"{titlesFoundPartially} titles from Wikipedia match partially");

            Console.WriteLine($"Precision: {precision}; Recall: {recall}");

            Console.ReadKey();
        }
    }
}
