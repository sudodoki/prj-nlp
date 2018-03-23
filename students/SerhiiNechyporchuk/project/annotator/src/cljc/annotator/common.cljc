(ns annotator.common)

(defn shared-fn
  "A function that is shared between clj and cljs"
  []
  (println "cljc!"))

(def number-of-attributes [3 4 5])

(def search-attributes
  [{:name "profile.firstName" :type "text" :example "Attendees with first name James"}
   {:name "profile.lastName" :type "text" :example "Attendees with last name Bond"}
   {:name "profile.company" :type "text" :example "Attendees who worked at Google, *Employees from Google"}
   {:name "profile.position" :type "text" :example "Attendees with CEO position, *Find all CEO"}
   {:name "profile.location" :type "text" :example "Attendees with Ukraine location, *Attendees from Ukraine"}
   {:name "profile.bio" :type "text" :example "1"}
   {:name "profile.enjoyment" :type "Sentiment" :example "Find all attendees with high enjoyment, Attendees who enjoyed AI Summit event"}
   {:name "profile.email" :type "text" :example "Attendees with abc.com in email"}
   {:name "profile.phone" :type "text" :example "1"}
   {:name "profile.website" :type "text" :example "1"}
   {:name "profile.linkedin" :type "bool" :example "Attendees with connected linkedin"}
   {:name "profile.google" :type "bool" :example "1"}
   {:name "profile.facebook" :type "bool" :example "1"}
   {:name "profile.twitter" :type "bool" :example "1"}
   {:name "post.text" :type "text" :example "Attendees who wrote post with Tesla, *Attendees who wrote post about Tesla"}
   {:name "post.count" :type "int" :example "Attendees who more than 10 posts"}
   {:name "post.likesCount" :type "int" :example "Attenddes who create post with more than 10 likes"}
   {:name "post.mentions" :type "Attendee" :example "Attendees who mentioned Elon Musk in post"}
   {:name "post.commentsCount" :type "int" :example "Attendees who create post with more than 10 comments"}
   {:name "post.sentiment" :type "Sentiment" :example "Attendees who wrote positive post with Tesla"}
   {:name "like.postText" :type "text" :example "Attendees who liked post with Tesla"}
   {:name "like.postAuthor" :type "Attendee" :example "Attendees who liked posts by Elon Musk"}
   {:name "like.postSentiment" :type "Sentiment" :example "Attendees who liked positive post with Tesla"}
   {:name "like.count" :type "int" :example "Attendees who made more than 30 likes"}
   {:name "comment.text" :type "text" :example "Attendees who wrote comment with Tesla"}
   {:name "comment.postText" :type "text" :example "Attendees who wrote comment with Tesla for the post with SpaceX"}
   {:name "comment.postAuthor" :type "Attendee" :example "Attenddes who wrote comment for Elon Musk post"}
   {:name "comment.postSentiment" :type "Sentiment" :example "Attendees who wrote comment for negative post with Boring"}
   {:name "poll.questionText" :type "text" :example "Attendees who answered for poll 'Are you happy?'"}
   {:name "poll.answerText" :type "text" :example "Attendees who answered Yes for poll 'Are you happy?'"}
   {:name "appSessions.count" :type "int" :example "Attendees with less than 10 app sessions"}
   {:name "pageView.content" :type "text" :example "Attendees who viewed page with SpaceX"}
   {:name "pageView.type" :type "PageType" :example "Attendees who viewed sponsor page"}
   {:name "pageView.count" :type "int" :example "Attendees who viewed sponsor page with Tesla more than 5 times"}
   {:name "ad.text" :type "text" :example "Attendees who clicked on ad with Tesla"}
   {:name "ad.sponsor" :type "Sponsor" :example "Attendees who clicked Apply ad with 'Wanna new iPhone?'"}
   {:name "ad.count" :type "int" :example "Attendees who clicked on ad more than 2 times"}]
  )