using System;
using System.Linq;
using HtmlAgilityPack;

namespace NLPCourse.DataTask.Acquisition02
{
    class Program
    {
        static void Main(string[] args)
        {
            Console.OutputEncoding = System.Text.Encoding.UTF8;

            Console.WriteLine("Scanning Львівський Форум / Видатні львів'яни...");
            Console.WriteLine();

            var homeUrl = "http://forum.lvivport.com/";
            var web = new HtmlWeb();
            var doc = web.Load(homeUrl.CombineVia("/", "forums/vidatni-lvivjani.69"));

            var topics = doc.DocumentNode
                .SelectNodes("//*/ol[@class=\"discussionListItems\"]/li/div/div/h3/a");

            int index = 1;
            foreach (var topic in topics) {
                var topicTitle = $"{index}. {topic.InnerText}";
                Console.WriteLine(topicTitle);
                Console.WriteLine('='.Replicate(topicTitle.Length));
                var topicUrl = homeUrl.CombineVia("/", topic.Attributes["href"].Value);

                PrintPosts(topicUrl);
                index++;

                //uncomment for testing purposes
                //if (index > 2) break;  
            }

#if DEBUG
            Console.ReadKey();
#endif
        }

        static void PrintPosts(string topicUrl) {
            var web = new HtmlWeb();
            var doc = web.Load(topicUrl);

            var posts = doc.DocumentNode
                .SelectNodes("//ol[@class=\"messageList\"]/li");

            foreach (var post in posts) {
                var authorNode = post.SelectSingleNode("./div[@class=\"messageUserInfo\"]/div/h3/a[@class=\"username\"]");
                if (authorNode != null) {
                    Console.WriteLine(authorNode.InnerText.ToUpperInvariant() + ":");

                    var textNode = post.SelectSingleNode(".//article/blockquote");
                    if (textNode != null) {
                        string postText = "";
                        foreach (var node in textNode.ChildNodes) {
                            if (node.NodeType == HtmlNodeType.Text) {
                                if (!string.IsNullOrEmpty(postText)) {
                                    postText += "\n  ";
                                }
                                postText += node.InnerText.Trim();
                            }
                        }
                        Console.WriteLine("  " + postText.Trim());
                    }
                    Console.WriteLine("  -------------------------------------------------------");
                    Console.WriteLine();
                }

            }


        }
    }
}
