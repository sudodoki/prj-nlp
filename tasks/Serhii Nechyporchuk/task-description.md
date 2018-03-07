# Create parser for queris in natural language to structured queries for event-tech domain
At Attenedify we have the tool to analyze you audience. It allows you to create structured queries using fairly complex UI and then execute them
using engine to retrieve attendees you care of. This queries have following form:

    (or (profile.email [contains "@abc.com"])
        (and (post.text [contains "Tesla"]
                        [sentiment "positive"])
             (like.postText [contains "SpaceX"])))
      

This query tries to find attendees that has '@abc.com' in theirs email or thoose who created posts with "Tesla" and positive sentiment and liked post with "SpaceX".
The idea is to reduce cognitive work for the users by providing an ability to create this queries using natural language (English specifically). It is obvious, that
it will be harder to handle such complex queries, but it will significally improve user experience for small queries (80% of user's needs).
Examples of such queries:

* `Find all attendees of AI Summit`
* `All profiles who posted content with 'machine learning'`
* `All profiles who liked content with 'nlp' and wrote positive posts with 'nlp'`
* `All profiles not enjoyed AI Summit or wrote negative post with 'event'`

## Description
Query parser must give an ability to search by this attributes:

| Attribute             | Type      | Description                             | Example query                                                                 |
|-----------------------|-----------|-----------------------------------------|-------------------------------------------------------------------------------|
| profile.firstName     | string    |                                         | Attendees with first name James                                               |
| profile.lastName      | string    |                                         | Attendees with last name Bond                                                 |
| profile.company       | string    |                                         | Attendees who worked at Google, *Employees from Google                        |
| profile.position      | string    |                                         | Attendees with CEO position, *Find all CEO                                    |
| profile.location      | string    |                                         | Attendees with Ukraine location, *Attendees from Ukraine                      |
| profile.bio           | string    |                                         |                                                                               |
| profile.enjoyment     | enum      | Average post sentiment across per event | Find all attendees with high enjoyment, Attendees who enjoyed AI Summit event |
| profile.email         | string    |                                         | Attendees with abc.com in email                                               |
| profile.phone         | string    |                                         |                                                                               |
| profile.website       | string    |                                         |                                                                               |
| profile.linkedin      | bool      |                                         | Attendees with connected linkedin                                             |
| profile.google        | bool      |                                         |                                                                               |
| profile.facebook      | bool      |                                         |                                                                               |
| profile.twitter       | bool      |                                         |                                                                               |
| post.text             | text      |                                         | Attendees who wrote post with Tesla, *Attendees who wrote post about Tesla    |
| post.count            | int       |                                         | Attendees who more than 10 posts                                              |
| post.likesCount       | int       |                                         | Attenddes with more than 10 likes on post                                     |
| post.mentions         | Attendee  |                                         | Attendees who mentioned Elon Musk in post                                     |
| post.commentsCount    | int       |                                         | Attendees with more than 10 comments on post                                  |
| post.sentiment        | Sentiment |                                         | Attendees who wrote positive post with Tesla                                  |
| like.postText         | text      |                                         | Attendees who liked post with Tesla                                           |
| like.postAuthor       | Attendee  |                                         | Attendees who liked posts by Elon Musk                                        |
| like.postSentiment    | Sentiment |                                         | Attendees who liked positive post with Tesla                                  |
| like.count            | int       |                                         | Attendees who made more than 30 likes                                         |
| comment.text          | text      |                                         | Attendees who wrote comment with Tesla                                        |
| comment.postText      | text      |                                         | Attendees who wrote comment with Tesla for the post with SpaceX               |
| comment.postAuthor    | Attendee  |                                         | Attenddes who wrote comment for Elon Musk post                                |
| comment.postSentiment | Sentiment |                                         | Attendees who wrote comment for negative post with Boring                     |
| poll.questionText     | text      |                                         | Attendees who answered for poll 'Are you happy?'                              |
| poll.answerText       | text      |                                         | Attendees who answered Yes for poll 'Are you happy?'                          |
| appSessions.count     | int       |                                         | Attenddees with less than 10 app sessions                                     |
| pageView.content      | text      |                                         | Attendees who viewed page with SpaceX                                         |
| pageView.type         | PageType  |                                         | Attendees who viewed sponsor page                                             |
| pageView.count        | int       |                                         | Attendees who viewed sponsor page with Tesla more than 5 times                |
| ad.text               | text      |                                         | Attendees who clicked on ad with Tesla                                        |
| ad.sponsor            | Sponsor   |                                         | Attendees who clicked Apply ad with 'Wanna new iPhone?'                       |
| ad.count              | int       |                                         | Attendees who clicke on ad more than 2 times                                  |

Описание типов и соотвествущих им операций

| Type   | Operations |
|--------|-----------|
| string | contains |

**SCOPE - EVENT, DATES**
