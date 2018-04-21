using System;
using System.Collections.Generic;
using System.Text;

namespace NLPCourse.Task5
{
    public class Feedback {
        public string Text;
        public int Rating = 0;
        public int SetimentOrigin => Rating == 3 ? 0 : (Rating > 3 ? 1 : -1);

        public string Lang { get; set; }

        public int SentimentPredicted = 0;
    }
}
