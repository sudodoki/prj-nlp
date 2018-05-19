using System;
using System.Collections.Generic;
using System.IO;
using System.Net.Http;
using System.Text;

using Newtonsoft.Json;

namespace NLPCourse.Task4
{
    public class TmdbReader
    {
        private static string _tmdbApiKey = "4447ebcd52f1e5080a5d7070db6ccd70";
        private static string _apiCreditsUrl = "http://api.themoviedb.org/3/person/{0}/combined_credits?api_key={1}";

        public IList<CastItem> GetActorCast(string actorId) {
            string url = string.Format(_apiCreditsUrl, actorId, _tmdbApiKey);

            var json = (new HttpClient()).GetStringAsync(url).Result;

            var credits = JsonConvert.DeserializeObject<ActorCredits>(json);

            return credits.Cast;
        }
    }

    public class CastItem {
        public string id;
        public string title;
        public string name;
        public string character;

        public string release_date;

        public string first_air_date;

        public bool IsPredicted = false;

        public double PredictedScore = 0; //0 - not found, 0.5 - partially, 1 - found

        public string FilmTitle {
            get {
                return !string.IsNullOrEmpty(title) ? title : name;
            }
        }

        public string FilmReleaseDate{
            get {
                return !string.IsNullOrEmpty(release_date) ? release_date : first_air_date;
            }
        }

    }

    public class ActorCredits {
        public IList<CastItem> Cast;
    }
}
