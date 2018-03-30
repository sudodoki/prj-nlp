# Create parser for queris in natural language to structured queries for event-tech domain
At Attenedify we have the tool to analyze your audience. It allows you to create structured queries using fairly complex UI and then execute them
using engine to retrieve attendees you care of. This queries have following form:

    (or (profile.email [contains "@abc.com"])
        (and (post.text [contains "Tesla"]
                        [sentiment "positive"])
             (like.postText [contains "SpaceX"])))
      

This query tries to find attendees that has '@abc.com' in theirs email or thoose who created positive post with "Tesla" and liked post with "SpaceX".
The idea of this task is to reduce cognitive work for the users by providing an ability to create this queries using natural language (English specifically). It is obvious, that
it will be harder to handle such complex queries, but it will significally improve user experience for small queries (80% of user's needs).
Examples of such queries:

* `Find all attendees of AI Summit`
* `All profiles who posted content with 'machine learning'`
* `All profiles who liked content with 'nlp' and wrote positive posts with 'nlp'`
* `All profiles not enjoyed AI Summit or wrote negative post with 'event'`

## Description
Query parser must give an ability to search by this attributes:

| Attribute             | Type      | Example query                                                                 |
|-----------------------|-----------|-------------------------------------------------------------------------------|
| profile.firstName     | text      | Attendees with first name James                                               |
| profile.lastName      | text      | Attendees with last name Bond                                                 |
| profile.company       | text      | Attendees who worked at Google, *Employees from Google                        |
| profile.position      | text      | Attendees with CEO position, *Find all CEO                                    |
| profile.location      | text      | Attendees with Ukraine location, *Attendees from Ukraine                      |
| profile.bio           | text      |                                                                               |
| profile.enjoyment     | Sentiment | Find all attendees with high enjoyment, Attendees who enjoyed AI Summit event |
| profile.email         | text      | Attendees with abc.com in email                                               |
| profile.phone         | text      |                                                                               |
| profile.website       | text      |                                                                               |
| profile.linkedin      | bool      | Attendees with connected linkedin                                             |
| profile.google        | bool      |                                                                               |
| profile.facebook      | bool      |                                                                               |
| profile.twitter       | bool      |                                                                               |
| post.text             | text      | Attendees who wrote post with Tesla, *Attendees who wrote post about Tesla    |
| post.count            | int       | Attendees who more than 10 posts                                              |
| post.likesCount       | int       | Attenddes who create post with more than 10 likes                             |
| post.mentions         | Attendee  | Attendees who mentioned Elon Musk in post                                     |
| post.commentsCount    | int       | Attendees who create post with more than 10 comments                          |
| post.sentiment        | Sentiment | Attendees who wrote positive post with Tesla                                  |
| like.postText         | text      | Attendees who liked post with Tesla                                           |
| like.postAuthor       | Attendee  | Attendees who liked posts by Elon Musk                                        |
| like.postSentiment    | Sentiment | Attendees who liked positive post with Tesla                                  |
| like.count            | int       | Attendees who made more than 30 likes                                         |
| comment.text          | text      | Attendees who wrote comment with Tesla                                        |
| comment.postText      | text      | Attendees who wrote comment with Tesla for the post with SpaceX               |
| comment.postAuthor    | Attendee  | Attenddes who wrote comment for Elon Musk post                                |
| comment.postSentiment | Sentiment | Attendees who wrote comment for negative post with Boring                     |
| poll.questionText     | text      | Attendees who answered for poll 'Are you happy?'                              |
| poll.answerText       | text      | Attendees who answered Yes for poll 'Are you happy?'                          |
| appSessions.count     | int       | Attendees with less than 10 app sessions                                      |
| pageView.content      | text      | Attendees who viewed page with SpaceX                                         |
| pageView.type         | PageType  | Attendees who viewed sponsor page                                             |
| pageView.count        | int       | Attendees who viewed sponsor page with Tesla more than 5 times                |
| ad.text               | text      | Attendees who clicked on ad with Tesla                                        |
| ad.sponsor            | Sponsor   | Attendees who clicked Apply ad with 'Wanna new iPhone?'                       |
| ad.count              | int       | Attendees who clicked on ad more than 2 times                                 |

Types and corresponding operations:

| Type                                  | Operations                   |
|---------------------------------------|------------------------------|
| text                                  | contains, not contain        |
| bool                                  | is, is not                   |
| int                                   | equals, more than, less than |
| Attendee                              | is, is not                   |
| Sentiment(Positive, Negative)         | is, is not                   |
| PageType (Schedule, Sponsor, Speaker) | is, is not                   |
| Sponsor                               | is, is not                   |

There is an ability to set scope of search:

* for event: `Find attendees who attended AI Summit`, `Find attendees who wrote post about neural nets at AI Summit`
* for date: `Find attendees who wrote post about AI since April 14`
* or both 
 
## Dataset
* Looks like I have to made it by myself
  * 2 corpora
    * corpus with shallow queries
    * corpus with deep (2-3 levels) queries
  * How to construct:
    * Construct every possibble way to phrase query for one attribute 
    * Construct every possibble way to phrase queries with several attributes that are highly correleated (sentiment and post text, etc)
    * Construct rest queries by combining previous 2

## Difficulties 
* There are many ways to phrase same query
* Need to construct your own corpus
* Posibility to implement `Did you mean ... ?` feature for error corrections or misleading queries
* Posibility to implement semantic search: `Find all attendees who posted about weather`
