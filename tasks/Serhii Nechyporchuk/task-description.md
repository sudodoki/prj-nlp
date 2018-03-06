# Движок для поиска на естественном языке интересующих документов в домене event-индустрии
Идея данной работы написать поисковый движок, который позволял бы делать поиск учасников конференций 
с помощью запросов на естественном языке, например: 
* `Find all attendees of AI Summit`
* `All profiles who posted content with 'machine learning'`
* `All profiles who liked content with 'nlp' and wrote positive posts with 'nlp'`
* `All profiles not enjoyed AI Summit or wrote negative post with 'event'`
* etc

## Описание
Данный движок должен иметь возмжоность искать по таким аттрибутам:

| Аттрибут  | Тип  |   |   |   |
|---|---|---|---|---|
| profile.firstName  |  string |   |   |   |
| profile.lastName | string  |   |   |   |
| profile.company | string  | 
| profile.position | string  | 
| profile.location | string  | 
| profile.bio | string  | 
| profile.enjoyment | enum  | 
| profile.email | string  | 
| profile.phone | string  | 
| profile.website | string  | 
| profile.linkedin | bool  | 
| profile.google | bool  | 
| profile.facebook | bool  | 
| profile.twitter | bool  | 
| post.text | text  | 
| post.count | int  | 
| post.likesCount | int  | 
| post.mentions | Attendee  |
| post.commentsCount | int |
| post.sentiment | Sentiment |
| like.postText | text |
| like.postAuthor | Attendee |
| like.postSentiment | Sentiment |
| like.count | int |
| comment.text | text |
| comment.postText | text |
| comment.postAuthor | Attendee |
| comment.postSentiment | Sentiment |
| poll.questionText | text |
| poll.answerText | text |
| appSessions.count | int |
| pageView.content | text |
| pageView.type | PageType |
| pageView.count | int |
| ad.text | text |
| ad.sponsor | Sponsor |
| ad.count | int |

Описание типов и соотвествущих им операций

| Type   | Operations |
|--------|-----------|
| string | contains |

**SCOPE - EVENT, DATES**
