# Fact checking in specific domain

## Problem formulation

Given sentence that features information about Actor taking part in Movie, output True / False and indicate which movie actor was in.

## Existing solutions

- based on unsupervised text matching https://arxiv.org/abs/1802.07398 / http://www.fakenewschallenge.org/#fnc-i-results / http://ranger.uta.edu/~cli/pubs/2015/claimbuster-cj15-hassan.pdf
- knowledge based https://arxiv.org/pdf/1708.07239.pdf
- rule based (probably, not much publications specific to fact checking) / IR kind approaches

## Possible datasets

Possible datasets are identified and prepared in [1-fact_check_ideation.ipynb](1-fact_check_ideation.ipynb).

## Challenges

- noisy data (always an excuse)
- lots of possible ways of phrasing same things
- phrases taken out of context are harder to analyze (pronouns, text about Elizabeth Taylor referencing to her as Liz, etc)

## Metrics

We are supposed to get accuracy / recall / F1.

# The attack vector

I decided to break task into 2 stages - step that would extract statements of pairs actor / movie from sentence (input text is split into sentence via SpaCy) and second step that would be returning True and set of matches for actor name / movie pairs.

## Finding all actors / movies

For first stage I took 150 sentences and annotated them with RELATED_ACTOR / RELATED_MOVIE labels for all occurences of actors, participating in movies (and actually, few instances of other media, like musicals and theater plays) and MISC_ACTOR / MISC_MOVIE. One thing I understood after I started working with annotated data – I should have labeled it differently to differentiate cases with multiple actors being in same / different movies (some labeling scheme with ACTOR_RELATED_1-MOVIE_RELATED_1, ACTOR_RELATED_2–MOVIE_RELATED_2).
On this subtask I tried assesing rules I came up with via precision/recall/f1 metrics with following assumptions:
- true positives = size of intersection between predicted related actors and ground truth related actors (compared by both label and span)
- false positive = count of predicted related actors which in fact were not
- false negative = count of related actors that were not marked as related (not sure, might have gone with count of actors marked as unrelated which were not those, but ¯\_(ツ)_/¯)
Progression:
1) I started with regexps for quoted text and Text That Was Like a Title, but I cannot restore them so moving to following step. Everything was marked as related, that could be considered baseline if it wasn't that dumb
2) Afterwards I used default NER from spacy medium model. WORK_OF_ART for movies and PERSON for actors (I also did ORG after seeing 'The Matrix' having 'Matrix' labeled as ORG, but dropped it as it gave quite a few false positives)
{'precision': 0.4897959183673469, 'recall': 0.5853658536585366, 'f1': 0.5333333333333333}
For inputs where NER worked well, I got good results
```
ORIG: Peter Fonda was in "Ulee's Gold"; "Fool's Gold" stars Kate Hudson daughter of Goldie Hawn
GOLD: [Peter Fonda]<-REL_ACT was in "[Ulee's Gold]<-REL_MOV"; "[Fool's Gold]<-REL_MOV" stars [Kate Hudson]<-REL_ACT daughter of [Goldie Hawn
OURS: [Peter Fonda]<-REL_ACT was in "[Ulee's Gold]<-REL_MOV"; "[Fool's Gold]<-REL_MOV" stars [Kate Hudson]<-REL_ACT daughter of [Goldie Hawn
ORIG: Before "The Blues Brothers", Dan Aykroyd & John Belushi starred in 1941 WWII farce
GOLD: Before "[The Blues Brothers]<-REL_MOV", [Dan Aykroyd]<-REL_ACT & [John Belushi]<-REL_ACT starred in 1941 WWII farce
OURS: Before "[The Blues Brothers]<-REL_MOV", [Dan Aykroyd]<-REL_ACT & [John Belushi]<-REL_ACT starred in 1941 WWII farce
```
But it didn't get unrelated actors / movies (duh) and some things it did get were not separated properly, for example
```
ORIG: "High Society" is a musical version of The Philadelphia Story Cary Grant-Jimmy Stewart-Katharine Hepburn classic
GOLD: "[High Society]<-MISC_MO" is a musical version of [The Philadelphia Story]<-REL_MOV [Cary Grant]<-REL_ACT-[Jimmy Stewart]<-REL_ACT-[Katharine Hepburn]<-REL_ACT classic
OURS: "High Society" is a musical version of [The Philadelphia Story Cary Grant-Jimmy Stewart-Katharine Hepburn]<-REL_MOV classic
```
it also had some false positives
```
GOLD: Of [James Cagney]<-MISC_AC, [Henry Fonda]<-MISC_AC or [Jack Lemmon]<-REL_ACT, the one [Jack Lemmon]<-REL_ACT won an Oscar for "[Mr. Roberts]<-REL_MOV"
OURS: Of [James Cagney]<-REL_ACT, [Henry Fonda]<-REL_ACT or [Jack Lemmon]<-REL_ACT, the one [Jack Lemmon]<-REL_ACT won an [Oscar]<-REL_MOV for "Mr. [Roberts]<-REL_ACT"
ORIG: Jason Scott Lee, who portrays Bruce Lee in Dragon: The Bruce Lee Story, is not related to Bruce Lee
GOLD: [Jason Scott Lee]<-REL_ACT, who portrays [Bruce Lee]<-MISC_AC in [Dragon: The Bruce Lee Story]<-REL_MOV, is not related to [Bruce Lee]<-MISC_AC
OURS: [Jason Scott Lee]<-REL_ACT, who portrays [Bruce Lee]<-REL_ACT in [Dragon:]<-REL_MOV [The Bruce Lee Story]<-REL_ACT, is not related to [Bruce Lee]<-REL_ACT
```
3) I tried using NER from [xx](https://spacy.io/models/xx#xx_ent_wiki_sm) model that was trained for NER on wikipedia but ran into issue in [spacy](https://github.com/explosion/spaCy/issues/1660) which was fixed later that force to use small english model along with xx ner. Just merging ners found by xx and small model improved result slightly. There was issue of some entities being differently classified. For example Daniel Day-Lewis being split into 2 names by small model bur correctly marked in xx model and some other instances. To avoid this I dropped all entities whose text was fully present in other found entity.
It gave following results:
{'precision': 0.4594594594594595, 'recall': 0.7700348432055749, 'f1': 0.5755208333333334}
precision dropped, but recall went up, which gave us better f1 score overall.
If we inspect output, we'll see better extraction of movies, as previously mostly only those in quotes were properly classified:
```
ORIG: The nude portrait of Kate Winslet in Titanic was actually drawn by Leonardo DiCaprio
GOLD: The nude portrait of [Kate Winslet]<-REL_ACT in [Titanic]<-REL_MOV was actually drawn by [Leonardo DiCaprio
OURS: The nude portrait of [Kate Winslet]<-REL_ACT in [Titanic]<-REL_MOV was actually drawn by [Leonardo DiCaprio
ORIG: In Harlem Nights 1989 film, Eddie Murphy was the adopted son of a 1930s nightclub owner played by Richard Pryor
GOLD: In [Harlem Nights]<-REL_MOV 1989 film, [Eddie Murphy]<-REL_ACT was the adopted son of a 1930s nightclub owner played by [Richard Pryor
OURS: In [Harlem Nights]<-REL_MOV 1989 film, [Eddie Murphy]<-REL_ACT was the adopted son of a 1930s nightclub owner played by [Richard Pryor
```
Top false positives were featuring cases of fictional characters from movies, quoted speech with author in parens, Oscar being classified as movie, some other corner cases:
```
ORIG: AMC's 'The Walking Dead', Rick, Carl, Daryl, Morgan, Carol and Maggie were introduced to us in Season 1.
GOLD: AMC's '[The Walking Dead]<-MISC_MO', Rick, [Carl]<-MISC_AC, Daryl, Morgan, Carol and Maggie were introduced to us in Season 1.
OURS: AMC's '[The Walking Dead]<-REL_MOV']<-REL_MOV, [Rick]<-REL_ACT, [Carl]<-REL_ACT, [Daryl]<-REL_ACT, [Morgan]<-REL_ACT, [Carol]<-REL_ACT and [Maggie]<-REL_ACT were introduced to us in Season 1.
ORIG: (Hi, I'm Leonard Maltin)  I like to think of Erin Brockovich 2000 Julia Roberts movie as an "Energetic, engaging David vs. Goliath story"
GOLD: (Hi, I'm Leonard Maltin)  I like to think of [Erin Brockovich]<-REL_MOV 2000 [Julia Roberts]<-REL_ACT movie as an "Energetic, engaging David vs. Goliath story"
OURS: (Hi, I'm [Leonard Maltin]<-REL_ACT) [ I]<-REL_ACT like to think of [Erin Brockovich]<-REL_MOV 2000]<-REL_ACT [Julia Roberts]<-REL_ACT movie as an "[Energetic]<-REL_ACT, engaging [David vs. Goliath]<-REL_MOV story"
ORIG: Liz won an Oscar for her role as a battlesome wife in Who's Afraid of Virginia Woolf? adaptation of an Albee play
GOLD: [Liz]<-REL_ACT won an Oscar for her role as a battlesome wife in [Who's Afraid of Virginia Woolf?]<-REL_MOV adaptation of an [Albee]<-MISC_AC play
OURS: [Liz]<-REL_ACT won an [Oscar]<-REL_MOV for her role as a battlesome wife in Who's Afraid of Virginia Woolf? adaptation of an Albee play
ORIG: Although Paul Giamatti lost the supporting actor Oscar for "Cinderella Man", he won the Emmy vote for "John Adams"
GOLD: Although [Paul Giamatti]<-REL_ACT lost the supporting actor Oscar for "[Cinderella Man]<-REL_MOV", he won the Emmy vote for "[John Adams]<-REL_MOV"
OURS: Although [Paul Giamatti]<-REL_ACT lost the supporting actor [Oscar]<-REL_MOV for "Cinderella Man", he won the Emmy vote for "John Adams"
ORIG: Of James Cagney, Henry Fonda or Jack Lemmon, the one Jack Lemmon won an Oscar for "Mr. Roberts"
GOLD: Of [James Cagney]<-MISC_AC, [Henry Fonda]<-MISC_AC or [Jack Lemmon]<-REL_ACT, the one [Jack Lemmon]<-REL_ACT won an Oscar for "[Mr. Roberts]<-REL_MOV"
OURS: Of [James Cagney]<-REL_ACT, [Henry Fonda]<-REL_ACT or [Jack Lemmon]<-REL_ACT, the one [Jack Lemmon]<-REL_ACT won an [Oscar]<-REL_MOV for "Mr. Roberts"
```
Now in false positive:
```
ORIG: Jane Russell & Marilyn Monroe sang about being "Two Little Girls from Little Rock" in Gentlemen Prefer Blondes 1953 movie musical
GOLD: [Jane Russell]<-REL_ACT & [Marilyn Monroe]<-REL_ACT sang about being "Two Little Girls from Little Rock" in [Gentlemen Prefer Blondes]<-REL_MOV 1953 movie musical
OURS: [Jane Russell & Marilyn Monroe]<-REL_ACT sang about being "[Two Little Girls from Little Rock]<-REL_MOV" in Gentlemen Prefer Blondes 1953 movie musical
ORIG: Kate Bosworth & James Marsden star in the 2011 remake of Straw Dogs Sam Peckinpah film
GOLD: [Kate Bosworth]<-REL_ACT & [James Marsden]<-REL_ACT star in the 2011 remake of [Straw Dogs]<-REL_MOV [Sam Peckinpah]<-MISC_AC film
OURS: [Kate Bosworth & James Marsden]<-REL_ACT star in the 2011 remake of [Straw Dogs Sam Peckinpah]<-REL_MOV film
ORIG: Like his character in "Parks and Recreation", Aziz Ansari was born in South Carolina.
GOLD: Like his character in "[Parks and Recreation]<-REL_ACT", [Aziz Ansari]<-REL_MOV was born in South Carolina.
OURS: Like his character in "[Parks and Recreation]<-REL_MOV", [Aziz Ansari]<-REL_ACT was born in South Carolina.
ORIG: Among the babes in "Babes in Toyland" are Ann Jillian as Bo Peep & Annette Funicello Mouseketeer as Mary Contrary
GOLD: Among the babes in "[Babes in Toyland]<-REL_MOV" are [Ann Jillian]<-REL_ACT as [Bo Peep]<-REL_ACT & [Annette Funicello Mouseketeer]<-REL_ACT as Mary Contrary
OURS: Among the babes in "[Babes in Toyland]<-REL_MOV" are [Ann Jillian]<-REL_ACT as Bo Peep & Annette [Funicello Mouseketeer]<-REL_ACT as [Mary Contrary
ORIG: Before "The Blues Brothers", Dan Aykroyd & John Belushi starred in 1941 WWII farce
GOLD: Before "[The Blues Brothers]<-REL_MOV", [Dan Aykroyd]<-REL_ACT & [John Belushi]<-REL_ACT starred in 1941 WWII farce
OURS: Before "[The Blues Brothers]<-REL_MOV", [Dan Aykroyd & John Belushi]<-REL_ACT starred in 1941 [WWII]<-REL_MOV farce
```
There's actually 2 instances of actor & actor being marked as single actor when it should have been 2 and also at least 2 issues in ground truth annotations

4) I decided it was high time to add some rules to find unrelated actors. I started with simple rule - for every token in sentence if it's in actor name, I try to find whether it's dependent on verb (or dependent on dependent on verb, up to chain of 3) whose lemma was in "direct", "don", "drop", "reject", "turn" (so, it was about director, don't/didn't do something, dropping /rejecting movie or turning it down - well, yeah and some other cases). I got 
{'precision': 0.4689507494646681, 'recall': 0.7630662020905923, 'f1': 0.5809018567639257}
with precision going slightly up and recall going slightly down (overall f1 ↑). I did have some other ideas for this part in mind, but main issue was misalignment of tokens from spacy and entities being single strings, which kinda forced me to throw something in without that much boost.

5) added filter for Oscar/Emmy (okay, only Oscar was showing up on false positives, but I just assumed, which is not a good thing for scientific way of doing things) not being movie / actor and got:
{'precision': 0.48026315789473684, 'recall': 0.7630662020905923, 'f1': 0.5895020188425303}
whith precision going up

6) filtered out actors that had "'s" in their text – ususally this referred to personal pronoun and actually more often marked director of the movieL
{'precision': 0.49547511312217196, 'recall': 0.7630662020905923, 'f1': 0.6008230452674898}

7) adding split for actor name 1 & actor name 2 actors duos gave me following result
{'precision': 0.5056179775280899, 'recall': 0.7839721254355401, 'f1': 0.6147540983606558}

8) and one thing I didn't do - assign unrelated movie label. I added rule to have all actors marked as unrelated if there's no movie candidated and to mark all movies unrelated if there's no related actors. Results are following:
{'precision': 0.5166240409207161, 'recall': 0.7038327526132404, 'f1': 0.5958702064896756}

Let's look at bottom of tp bucket:
```
ORIG: Schwarzenegger is a Soviet cop teamed with James Belushi's Chicago cop in Red Heat action movie
GOLD: [Schwarzenegger]<-REL_ACT is a Soviet cop teamed with [James Belushi]<-REL_ACT's Chicago cop in [Red Heat]<-REL_MOV action movie
OURS: Schwarzenegger is a [Soviet]<-MISC_MO cop teamed with James Belushi's Chicago cop in [Red Heat]<-MISC_MO action movie
ORIG: This "Alien" actress starred in her long-time pal Christopher Durang's 1996 play "Sex and Longing"
GOLD: This "[Alien]<-MISC_MO" actress starred in her long-time pal [Christopher Durang]<-MISC_AC's 1996 play "[Sex and Longing]<-MISC_MO"
OURS: This "Alien" actress starred in her long-time pal Christopher Durang's 1996 play ["Sex and Longing"
ORIG: Quentin Tarantino directed Pulp Fiction film & also had a bit role as Jimmy of Toluca Lake
GOLD: [Quentin Tarantino]<-REL_ACT directed [Pulp Fiction]<-REL_MOV film & also had a bit role as Jimmy of Toluca Lake
OURS: [Quentin Tarantino]<-MISC_AC directed [Pulp Fiction]<-MISC_MO film & also had a bit role as [Jimmy of Toluca Lake
ORIG: John Larroquette played Captain Stillman in Stripes wacky 1981 comedy about misfits in the Army
GOLD: [John Larroquette]<-REL_ACT played Captain Stillman in [Stripes]<-REL_MOV wacky 1981 comedy about misfits in the Army
OURS: [John Larroquette]<-MISC_AC played [Captain Stillman]<-MISC_AC in Stripes wacky 1981 comedy about misfits in the Army
```
Funny thing is our rule on directors backfired here as Quentin Tarantino, being director, did have part in his movie.
This might be mitigated by adding rule to check coordinated verbs along those indicating unrelatedness.
Other type of error
```
ORIG: AMC's 'The Walking Dead', Rick, Carl, Daryl, Morgan, Carol and Maggie were introduced to us in Season 1.
GOLD: AMC's '[The Walking Dead]<-MISC_MO', Rick, [Carl]<-MISC_AC, Daryl, Morgan, Carol and Maggie were introduced to us in Season 1.
OURS: AMC's '[The Walking Dead]<-REL_MOV']<-REL_MOV, [Rick]<-REL_ACT, [Carl]<-REL_ACT, [Daryl]<-REL_ACT, [Morgan]<-REL_ACT, [Carol]<-REL_ACT and [Maggie]<-REL_ACT were introduced to us in Season 1.
ORIG: (Hi, I'm Leonard Maltin)  I like to think of Erin Brockovich 2000 Julia Roberts movie as an "Energetic, engaging David vs. Goliath story"
GOLD: (Hi, I'm Leonard Maltin)  I like to think of [Erin Brockovich]<-REL_MOV 2000 [Julia Roberts]<-REL_ACT movie as an "Energetic, engaging David vs. Goliath story"
OURS: (Hi, I'm [Leonard Maltin]<-REL_ACT) [ I]<-REL_ACT like to think of [Erin Brockovich]<-REL_MOV 2000]<-REL_ACT [Julia Roberts]<-REL_ACT movie as an "[Energetic]<-REL_ACT, engaging [David vs. Goliath]<-REL_MOV story"
ORIG: (<a href="http://www.j-archive.com/media/2008-03-31_J_02.jpg" target="_blank">Drew Barrymore delivers the clue.</a>)  In my film "Music and Lyrics", Hugh Grant plays the "Jeopardy! Think Music" written by Merv Griffin creator of "Jeopardy!"
GOLD: (<a href="http://www.j-archive.com/media/2008-03-31_J_02.jpg" target="_blank">[Drew Barrymore]<-MISC_AC delivers the clue.</a>)  In my film "[Music and Lyrics]<-REL_MOV", [Hugh Grant]<-REL_ACT plays the "[Jeopardy! Think Music]<-MISC_MO" written by [Merv Griffin]<-MISC_AC creator of "[Jeopardy!]<-MISC_MO"
OURS: (<a href="http://www.j-archive.com/media/2008-03-31_J_02.jpg" target="_blank">[Drew Barrymore]<-REL_ACT delivers the clue.</a>)  [In my film]<-REL_MOV "[Music and Lyrics]<-REL_MOV", [Hugh Grant]<-REL_ACT plays the "[Jeopardy]<-REL_MOV! Think Music]<-REL_MOV" written by [Merv Griffin]<-REL_ACT creator of "Jeopardy!"
ORIG: Committed to TV, Tom Selleck had to turn down Indiana Jones role in "Raiders of the Lost Ark" (curse you, Hawaiian shirt!)
GOLD: Committed to TV, [Tom Selleck]<-MISC_AC had to turn down Indiana Jones role in "[Raiders of the Lost Ark]<-MISC_MO" (curse you, Hawaiian shirt!)
OURS: [Committed to TV]<-REL_MOV, [Tom Selleck]<-REL_ACT had to turn down [Indiana Jones]<-REL_ACT role in "[Raiders of the Lost Ark]<-REL_MOV" (curse you, [Hawaiian]<-REL_MOV shirt!)
ORIG: He played Elvis' trainer in "Kid Galahad" a "Dirty Dozen" years before Charles Bronson starred in "Death Wish"
GOLD: He played Elvis' trainer in "[Kid Galahad]<-MISC_MO" a "[Dirty Dozen]<-MISC_MO" years before [Charles Bronson]<-REL_ACT starred in "[Death Wish]<-REL_MOV"
OURS: He played [Elvis]<-REL_ACT' trainer in "[Kid Galahad]<-REL_MOV" a "Dirty Dozen" years before Charles Bronson starred in "Death Wish"
```
Can possibly come up with few simple rules, like removing personal pronouns or 'in % film'. But most errors could be accounted for characters mistakenly interpreted as movies / actors. I would stop working on rules here. And started considering adding RELATION_TYPE or similar to manually annotated dataset.

One other thing before doing fact check was trying to have movie - actor statements, as single actor could be in different movies (or multiple actors in single movies, or multiple actors in multiple movies - be it together or each in own set). For this I assumed semi colon would split such statements in two. Otherwise going from left to right, after I found related movie or related actor, I would remember those. If I already have movie and actor, I'm pushing those to result. One other thing was avoiding movie and actor being the same / overlapping token (which could happen), so added that check as well.

## Finding actors / movies by name

Given name found by NER (it could be not the full name surname pair), I would try to match it against existing records of actors with following assumptions:
- there are some nicknames for proper names (Henry -> Hank, etc) which might be used
- initials can be used to reference actor (not full initials were used, though, didn't have that info)
- dot in initials might have been omitted
- people could be referenced by last name only

Afterwards I tried validating end 2 end on statements from different sources.
Highlight of the test are following
```
Michael Keaton played Spiderman villain in the 2017 superhero movie "Spiderman: Homecoming": 
Spiderman starred in Spiderman: Homecoming -> This might be not true. 

In the 1997 American science fiction comedy Men in Black, Tommy Lee Jones played Agent K: 
Tommy Lee Jones starred in Men in Black -> Yes, it's true. See https://www.imdb.com/title/tt0119654
Tommy Lee Jones starred in Agent K -> This might be not true.

Jim Carrey and Renee Zellweger starred in Me, Myself & Irene, released in 2000, about a cop with dissociative identity disorder: 
Renee Zellweger starred in Me -> This might be not true.
Renee Zellweger starred in Myself & Irene -> This might be not true.

Every character in "The Wizard of Oz" wanted something different from the Wizard.: 
Wizard starred in The Wizard of Oz -> This might be not true.

Faye Dunaway was considered for Elaine Robinson in 'The Graduate' (1967), but she had to turn it down in order to star in Bonnie and Clyde (1967): 
Elaine Robinson starred in 'The Graduate' -> This might be not true.
Elaine Robinson starred in The Graduate -> This might be not true.
Bonnie and Clyde starred in The Graduate -> This might be not true.

Heath Ledger dated his 'Brokeback Mountain' (2005) co-star Michelle Williams: 
Heath Ledger starred in Brokeback Mountain -> Yes, it's true. See https://www.imdb.com/title/tt0388795
Michelle Williams starred in Brokeback Mountain -> Yes, it's true. See https://www.imdb.com/title/tt0388795

Ben Affleck was originally going to direct 'Gone Girl' (2014) along with starring in it: 
Ben Affleck starred in Gone Girl -> Yes, it's true. See https://www.imdb.com/title/tt2267998

Cameron Diaz did her own singing in 'The Mask' (1994): 
Cameron Diaz starred in The Mask' -> This might be not true.

Vivian Leigh disliked kissing Clark Gable in 'Gone With the Wind' (1939) because he had bad breath: 
Clark Gable starred in Gone With the Wind -> This might be not true.

Christopher Plummer and Julie Andrews had an affair during the filming of 'The Sound of Music' (1965): 
Julie Andrews starred in 'The Sound of Music' -> This might be not true.
Julie Andrews starred in The Sound of Music -> Yes, it's true. See https://www.imdb.com/title/tt0059742

To break the ice for the nude scene, Kate Winslet flashed Leonardo Dicaprio before shooting began on 'Titanic' (1997): 
Leonardo Dicaprio starred in Titanic -> This might be not true.

Denzel Washington was originally going to play Jules in 'Pulp Fiction' (1994) but dropped out due to creative differences with director Quentin Tarantino: 
Denzel Washington starred in Jules in 'Pulp Fiction' -> This might be not true.
Jules starred in Pulp Fiction -> This might be not true.
Quentin Tarantino starred in Pulp Fiction -> Yes, it's true. See https://www.imdb.com/title/tt0110912

After his tour of duty in the Vietnam War ended in 1968, Oliver Stone wrote a screenplay called Break, a semi-autobiographical account detailing his experiences with his parents and his time in the Vietnam War.: 
Oliver Stone starred in Vietnam War -> This might be not true.
Oliver Stone starred in Break -> This might be not true.
Oliver Stone starred in Vietnam War -> This might be not true.
James Woods, who had starred in Stone's film Salvador: 
Stone starred in Salvador -> This might be not true.
```
Main issues here are because of some movies having extra quotes in the beginning or end which coul be removed. Some issues are accounting for wrong thing identified as movies / actors and once again roles confusing classifier.

On end to end, with on set of 30 sentences I haven't seen previously with 16 being positive and 14 negative I evaluated e2e system. I considered it a match if at least single statement was confirmed in statement labeled with True (of 0 and False otherwise), I get following metrics from the standpoint of True class:
```
Accuracy:  0.733333333333
Precision:  0.9
Recall:  0.5625
F1:  0.692307692308
```

## What to improve

Overall false positives could be reduced by checking named entities found against character played by person (this data was available in original dataset) or filtering possible actor entities to only those found in database. Splitting stages seemed like a good idea, but a lot of information for splitting statements and finding corresponding actors is lost. There's much more that could be obtained from deps.