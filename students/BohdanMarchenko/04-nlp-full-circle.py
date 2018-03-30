import requests
from bs4 import BeautifulSoup
import spacy

class DataProcessor(object):
    nlp = spacy.load("en")
    truth_page = "http://www.imdb.com/name/nm0000704/"
    wiki_page = "https://en.wikipedia.org/wiki/Elijah_Wood"
    result = []

    def get_truth(self):
        result = []
        response = requests.get(self.truth_page)
        soup = BeautifulSoup(response.text, 'html5lib').find("div", {"class": "filmo-category-section"})
        movies = soup.find_all("div", {"class": "filmo-row"})
        for movie in movies:
            year = movie.find("span", {"class": "year_column"}).text.strip()
            name = movie.find("b").text
            result.append((name, year))
        self.truth = result
    #this is the result of teh above method, a bit cleaned manually e.g. '2015/I' replace to '2015'
    truth = (("Dirk Gently's Holistic Detective Agency", '2016-2017'),
        ("I Don't Feel at Home in This World Anymore.", '2017'),
        ('The Trust', '2016'),
        ('The Last Witch Hunter', '2015'),
        ('Henry', '2015'),
        ('Over the Garden Wall', '2014'),
        ('Wilfred', '2011-2014'),
        ('Set Fire to the Stars', '2014'),
        ('Open Windows', '2014'),
        ('Broken Age', '2014'),
        ('Cooties', '2014'),
        ('This Must Be the Only Fantasy', '2013'),
        ('Tome of the Unknown', '2013'),
        ('Setup, Punch', '2013'),
        ('Grand Piano', '2013'),
        ('Kaze tachinu', '2013'),
        ('Pawn Shop Chronicles', '2013'),
        ('Peter Panzerfaust', '2013'),
        ('TRON: Uprising', '2012-2013'),
        ('The Narrative of Victor Karloch', '2012'),
        ('The Hobbit: An Unexpected Journey', '2012'),
        ('Red vs. Blue', '2012'),
        ('The Ballad of Danko Jones', '2012'),
        ('Maniac', '2012'),
        ('Revenge for Jolly!', '2012'),
        ('Treasure Island', '2012'),
        ('Celeste & Jesse Forever', '2012'),
        ('The Death and Return of Superman', '2011'),
        ('Happy Feet Two', '2011'),
        ('Robot Chicken', '2006-2011'),
        ('Beastie Boys: Make Some Noise', '2011'),
        ('I Think Bad Thoughts', '2011'),
        ('Boobie', '2011'),
        ('Funny or Die Presents...', '2011'),
        ('Beastie Boys: Fight for Your Right Revisited', '2011'),
        ('Glenn Martin DDS', '2010'),
        ('WWII in HD: The Air War', '2010'),
        ('Full of Regret', '2010'),
        ('Family Guy', '2010'),
        ('God of War III', '2010'),
        ('The Romantics', '2010'),
        ('Beyond All Boundaries', '2009'),
        ('Saturday Night Live', '2009'),
        ("9", '2009'),
        ('The Legend of Spyro: Dawn of the Dragon', '2008'),
        ('The Oxford Murders', '2008'),
        ('The Legend of Spyro: The Eternal Night', '2007'),
        ('Day Zero', '2007'),
        ('The Lord of the Rings: The Battle for Middle-earth II - The Rise of the Witch-king', '2006'),
        ('American Dad!', '2006'),
        ('Happy Feet ', '2006'),
        ('Happy Feet', '2006'),
        ('Freak Show', '2006'),
        ('The Legend of Spyro: A New Beginning', '2006'),
        ('Bobby', '2006'),
        ("Paris, je t'aime", '2006'),
        ('Everything Is Illuminated', '2005'),
        ("I'm Still Here: Real Diaries of Young People Who Lived During the Holocaust", '2005'),
        ('Sin City', '2005'),
        ('Green Street', '2005'),
        ('The Lord of the Rings: The Battle for Middle-Earth', '2004'),
        ('King of the Hill', '2004'),
        ('Eternal Sunshine of the Spotless Mind', '2004'),
        ('The Lord of the Rings: The Return of the King', '2003'),
        ('Spy Kids 3: Game Over', '2003'),
        ('Storyline Online', '2003'),
        ('The Lord of the Rings: The Two Towers', '2002'),
        ('Franklin', '2002'),
        ('Try Seventeen', '2002'),
        ('The Adventures of Tom Thumb & Thumbelina', '2002'),
        ('Ash Wednesday', '2002'),
        ('The Lord of the Rings: The Fellowship of the Ring', '2001'),
        ('Chain of Fools', '2000'),
        ('Black and White', '1999'),
        ('The Bumblebee Flies Anyway', '1999'),
        ('The Faculty', '1998'),
        ('Deep Impact', '1998'),
        ('The Wonderful World of Disney', '1997'),
        ('The Ice Storm', '1997'),
        ('Homicide: Life on the Street', '1996'),
        ('Adventures from the Book of Virtues', '1996'),
        ('Flipper', '1996'),
        ('The Cranberries: Ridiculous Thoughts', '1995'),
        ('The War', '1994'),
        ('North', '1994'),
        ('Frasier', '1994'),
        ('The Good Son', '1993'),
        ('The Adventures of Huck Finn', '1993'),
        ('The Witness', '1993'),
        ('Forever Young', '1992'),
        ('Day-O', '1992'),
        ('Radio Flyer', '1992'),
        ('Paradise', '1991'),
        ('Avalon', '1990'),
        ('Child in the Night', '1990'),
        ('Internal Affairs', '1990'),
        ('Back to the Future Part II', '1989')
    )

    def scrape_wikipedia(self):
        response = requests.get(self.wiki_page)
        soup = BeautifulSoup(response.text, 'html5lib')
        sd = soup.find("div", {"class": "mw-parser-output"})
        self.text =  " ".join([i.text for i in sd.findAll("p")])

    #this is the result of the above method
    text = """Elijah Jordan Wood (born January 28, 1981) is an American actor, voice actor, DJ, and producer.
        He is best known for his high-profile leading role as Frodo Baggins in Peter Jackson's epic film trilogy
        The Lord of the Rings (2001–03).
        Wood made his film debut with a minor part in Back to the Future Part II (1989).
        Landing a succession of larger roles, he was critically acclaimed as a child actor by age nine, being nominated for several Young Artist Awards.
        Notable films during this period include Avalon (1990), Paradise (1991), Radio Flyer, Forever Young (both 1992), The Adventures of Huck Finn and The Good Son (both 1993).
        He then transitioned into teenage roles with the films North, The War (both 1994), Flipper (1996), The Ice Storm (1997), Deep Impact and The Faculty (both 1998).
        Following The Lord of the Rings, he has chosen varied roles in films such as Eternal Sunshine of the Spotless Mind (2004), Sin City, Green Street,
        Everything Is Illuminated (all 2005), Paris, je t'aime, Bobby (both 2006), Celeste and Jesse Forever, Maniac (both 2012), Grand Piano (2013), The Last Witch Hunter (2015), The Trust (2016), and I Don't Feel at Home in This World Anymore (2017).
        Wood provided the voice of Mumble in the animated films Happy Feet (2006) and Happy Feet Two (2011), as well as the eponymous lead in the Tim Burton-produced 9 (2009).
        He was the voice of Spyro in the Legend of Spyro videogame trilogy (2006–08).
        He also provided the voice of Beck on the Disney XD series Tron: Uprising (2012–13), and starred as Wirt in the Cartoon Network miniseries Over the Garden Wall (2014).
        From 2011–14, Wood played the role of Ryan Newman on FX's dark comedy, Wilfred, for which he received a nomination for the Satellite Award for Best Actor – Television Series Musical or Comedy.
        As of 2016, he co-stars on the BBC America series
        Dirk Gently's Holistic Detective Agency.
        Wood has his own record label, Simian Records, which he founded in 2005.
        In 2010, he founded the production company SpectreVision, which specializes in producing horror films.
        Wood was born on January 28, 1981, in Cedar Rapids, Iowa, the second of three children.[1]
        His parents, Debbie (née Krause) and Warren Wood, operated a delicatessen.[2]
        He was raised Roman Catholic.[3][4][5] He has an older brother, Zachariah,[6] and a younger sister, Hannah.[7]
        At age seven, Wood began modeling in his hometown and took piano lessons.[8][9]
        In elementary school, he appeared in The Sound of Music and played the title character in The Wonderful Wizard of Oz.
        He also served as choir boy in a Marion Creative Council production of See How They Run.
        In 1989, his parents sold their delicatessen and the family–without his father–moved to Los Angeles for Wood to pursue an acting career.
        His parents divorced when he was 15.[10]
        Wood modeled and appeared in local commercials.
        He got his first break in the music video for Paula Abdul's "Forever Your Girl", directed by David Fincher.
        This was followed by a pivotal role in the made-for-TV film, Child in the Night, and a minor role in Back to the Future Part II.
        Nine-year-old Wood auditioned for a role in Kindergarten Cop, but was told by director Ivan Reitman that his performance was not believable, which Wood later said was "a harsh thing to say to a nine-year-old".[11] Playing Aidan Quinn's son in Avalon garnered professional attention for Wood; the film received widespread critical acclaim and was nominated for four Academy Awards.
        A small part in Richard Gere's Internal Affairs was followed by the role of a boy who brings estranged couple Melanie Griffith and Don Johnson back together in Paradise (1991).
        In 1992, Wood co-starred with Mel Gibson and Jamie Lee Curtis in Forever Young, and with Joseph Mazzello in Radio Flyer.
        In 1993, Wood played the title character in Disney's adaptation of Mark Twain's novel, The Adventures of Huck Finn, and co-starred with Macaulay Culkin in the psychological thriller The Good Son.
        The following year, he starred in The War, alongside Kevin Costner.
        Roger Ebert's review of the film praised Wood highly, stating that Wood "has emerged, I believe, as the most talented actor, in his age group, in Hollywood history".[12] Wood's title role–opposite Bruce Willis–in the Robert Reiner film North (1994) was followed by a Super Bowl commercial for Lay's "Wavy" potato chips (with Dan Quayle).[13]
        In 1995, Wood appeared in the music video for The Cranberries' "Ridiculous Thoughts", played the lead role in Flipper, and co-starred in Ang Lee's critically acclaimed The Ice Storm.
        In 1997, Wood played Jack "The Artful Dodger" Dawkins in a made-for-TV adaptation of Oliver Twist, alongside Richard Dreyfuss.
        The following year, he had a leading role in the sci-fi disaster film Deep Impact, and a supporting role in The Faculty, directed by Robert Rodriguez.
        In 1999, Wood played a suburban white teenager who affects hip-hop lingo in James Toback's Black and White, and a junior hitman in Chain of Fools.
        Wood was cast as Frodo Baggins in The Lord of the Rings:
        The Fellowship of the Ring, the first installment of director Peter Jackson's adaptation of J. R. R. Tolkien's multi-volume novel.
        This gave Wood top billing as Baggins, alongside a cast that included Ian McKellen, Liv Tyler, Orlando Bloom, Cate Blanchett, Christopher Lee, Sean Bean, Sean Astin, Billy Boyd, Dominic Monaghan, Viggo Mortensen, and John Rhys-Davies.
        The Lord of the Rings trilogy was filmed in New Zealand, in a process taking more than one year for principal photography alone, with pick-up shots occurring annually for the next four years.
        Before the cast left the country, Jackson gave Wood two gifts: one of the One Ring props used on the set and Sting, Frodo's sword.
        He was also given a pair of prosthetic "hobbit feet" of the type worn during filming.
        Fellowship was released in 2001 and went on to gross more than $870 million at the worldwide box office.
        In 2002, Wood lent his voice to the DTV release of The Adventures of Tom Thumb and Thumbelina.
        Later that year, the second part of Peter Jackson's trilogy was released, titled The Lord of the Rings:
        The Two Towers.
        The film grossed $926 million at the worldwide box office.
        In 2003, Wood starred in the DTV film
        All I Want.
        The concluding chapter of the Rings trilogy, The Lord of the Rings:
        The Return of the King was unveiled that December, and grossed in excess of $1.1 billion at the worldwide box office.
        At the 76th Academy Awards, the film received all 11 Academy Awards for which it was nominated, therefore holding the record for highest Oscar sweep.
        Wood's first role following his Lord of the Rings success was in Eternal Sunshine of the Spotless Mind (2004), in which he played Patrick, an unscrupulous lab technician who pursues Kate Winslet.
        The film received the Academy Award for Best Original Screenplay in 2005.
        He next played the serial killer Kevin in Robert Rodriguez's adaptation of Frank Miller's comic book series, Sin City (2005).
        On May 12, 2005, Wood hosted MTV Presents:
        The Next Generation Xbox Revealed for the launch of the Xbox 360 games console.[14]
        In Everything Is Illuminated (2005), Wood starred as a young Jewish-American man on a quest to find the woman who saved his grandfather during World War II.
        It was based on the novel of the same name by Jonathan Safran Foer.
        In Green Street (also 2005), he played an American college student who joins a violent British football firm.
        Both had limited release but were critically acclaimed.
        Wood shot a small part in Paris, je t'aime (2006), which consists of 18 five-minute sections, each directed by a different director.
        Wood's section, called "Quartier de la Madeleine", was directed by Vincenzo Natali.
        The film played at the Cannes Film Festival and the Toronto International Film Festival.[15]
        In George Miller's animated musical Happy Feet (2006), Wood provided the voice of Mumble, a penguin who can tap dance, but not sing.[16]
        Happy Feet grossed over $380 million worldwide, and received both the Academy Award for Best Animated Feature and the BAFTA Award for Best Animated Film.
        Wood reprised his role for the film's sequel, Happy Feet Two (2011).
        Also in 2006, he was part of the ensemble cast of Emilio Estevez's drama Bobby, a fictionalized account of the hours leading up to the June 5, 1968, shooting of U.S. Senator Robert F. Kennedy.
        In the film, Wood marries Lindsay Lohan's character in order to avoid being drafted for the Vietnam War.[17]
        Bobby screened in competition at the Venice Film Festival.
        Wood—along with his co-stars—received a nomination for the Screen Actors Guild Award for Outstanding Performance by a Cast in a Motion Picture.
        Later that year, Wood hosted the television special Saving a Species: The Great Penguin Rescue for Discovery Kids;[18] he received a nomination for the Daytime Emmy Award for Outstanding Performer in Children's Programming.[19]
        That same year, it was announced that Wood was set to star in a biographical film about singer Iggy Pop, putatively named The Passenger.[20]
        However, the project failed to come to fruition after years in development.[21]
        On January 4, 2007, Wood joined Screen Actors Guild president Alan Rosenberg in a live telecast to announce the nominees for the 13th Annual Screen Actors Guild Awards.[22]
        Later that year, he starred in Day Zero, a drama about conscription in the United States, which had its debut at the Tribeca Film Festival.[23]
        In The Oxford Murders (2008), a film adaptation of the novel of the same name by Guillermo Martínez, Wood played a graduate student who investigates a series of bizarre, mathematically-based murders in Oxford.[24]
        The following year, he voiced the lead in the animated feature film 9,[25] which was produced by Tim Burton.
        Wood's first starring television role came on the FX series, Wilfred, where he played Ryan Newman.
        The pilot was shot in the summer of 2010,[26] and the series lasted four seasons, with the final episode airing in the U.S. on August 13, 2014.
        For his role, Wood received a nomination for the Satellite Award for Best Actor – Television Series Musical or Comedy in 2011.
        In January 2011, it was confirmed that Wood had signed on to reprise the role of Frodo Baggins in The Hobbit:
        An Unexpected Journey, the first film of the Hobbit trilogy, directed again by Peter Jackson.[27]
        The film was released the following year and grossed over $1 billion at the worldwide box office.[28]
        Also in 2011, Wood featured in the Beastie Boys' music video for "Make Some Noise", along with Seth Rogen and Danny McBride.
        He then starred in the Flying Lotus music video "Tiny Tortures," where he played a recent amputee coming to grips with his new situation.
        The psychedelic video was described as "menacing and magical".[29]
        In 2012, Wood had a supporting role in the romcom Celeste and Jesse Forever, and starred in the horror film Maniac, for which he received the Fangoria Chainsaw Award for Best Actor.
        In 2013, Wood played the leading role in the Hitchcockian suspense thriller Grand Piano.
        Next he provided the voice of main character Wirt in Cartoon Network's animated miniseries Over the Garden Wall.[30]
        The series collected three Primetime Emmy Awards in 2015, including Outstanding Animated Program.[31]
        This was followed by prominent roles in films The Last Witch Hunter (2015), opposite Vin Diesel; The Trust (2016), opposite Nicolas Cage; and I Don't Feel at Home in This World Anymore (2017), opposite Melanie Lynskey.
        The latter film was awarded the U.S. Dramatic Grand Jury Prize at Sundance in January 2017.[32]
        Since 2016, Wood has been co-starring with Samuel Barnett as Todd Brotzman on the BBC America series
        Dirk Gently's Holistic Detective Agency.
        In 2005, Wood started his own record label called Simian Records.
        On September 19, 2006, Wood announced that Simian had signed The Apples in Stereo as their first band, with their new album New Magnetic Wonder released in February 2007.[33]
        In addition, he also directed the music video for "Energy".[34]
        The other band signed to Simian thus far is Heloise and the Savoir Faire.
        In 2006, Wood appeared on the second season of American Dad!
        (episode #6).
        He was the voice of Ethan, a college student and Haley's crush, who listens to Professor Baxter's (Roger) advice about following your heart and doing something new.
        Ethan takes this the wrong way and stabs his father 38 times (not shown) and becomes very crazy.
        Roger admits to being a fake, and he and Hayley leave, saying that the police are coming.
        This suggests that Ethan gets arrested, and is out of the show.
        Wood does not appear again.
        At PAX East in April 2012, it was confirmed that Wood would provide the voice of Sigma on Season 10 of the machinima TV series Red vs. Blue.
        Wood has also provided voiceovers for video games, including the voice of Spyro the Dragon in the Legend of Spyro game trilogy,[35] as well as reprising Mumble in the game version of Happy Feet.[36]
        He also contributed his talents to fellow Lord of the Rings star Viggo Mortensen's album Pandemoniumfromamerica, singing and playing various instruments on the album.[37]
        On April 11, 2008, Wood was the guest host of Channel 4's Friday Night Project.
        On April 25, 2009, Wood was honored with the Midnight Award by the San Francisco International Film Festival as an American actor who "has made outstanding contributions to independent and Hollywood cinema, and who brings striking intelligence, exemplary talent and extraordinary depth of character to his roles".[38]
        Wood also starred in an episode of Yo Gabba Gabba!, entitled "Eat," where he danced and "went crazy" alongside the rest of the Yo Gabba Gabba! cast.
        Wood can be seen in a short film on stepthroughtheportal.com.
        The site is an interactive website created by Simian Records artists The Apples in Stereo, promoting their album, Travelers in Space and Time.
        In addition to producing and acting, Wood has become a well-known DJ.
        Together with his friend Zach Cowie, they formed Wooden Wisdom and has toured around the world.
        They have spun at events such as the Bushmills Live 2012 festival at the Old Bushmills Distillery, the opening of the Brickell City Centre and at the pre-Emmy party at The London West Hollywood hotel.[39][40]
        Despite the common misconception, Wood has confirmed that his DJ name is just his name, and not "DJ Frodo" as has been falsely spread by the internet.[41]
        Wood has signed up to co-produce and also star in the film Black Wings
        Has My Angel, based on the noir novel of the same name, with Anthony Moody and Rob Malkani of Indalo Productions.
        Wood, alongside Tom Hiddleston and Anna Paquin, started shooting in late 2012.[42]
        In 2010, Wood, together with Daniel Noah and Josh C. Waller, founded The Woodshed, a production company that promotes horror films.
        In 2013, the company was re-branded as SpectreVision.[43]
        In 2010, Wood starred in The Apples in Stereo's music video for their song "Dance Floor".[44]
        In 2010, Wood also starred alongside Lemmy and Selma Blair in the short film/music video Full of Regret for Canadian rock band Danko Jones.
        Wood voiced Shay, one of two main characters in the adventure game Broken Age, for which he received the 2014 Performance in a Comedy, Lead award from National Academy of Video Game Trade Reviewers (NAVGTR).[45] Wood's most dedicated charitable commitment is to The Art of Elysium that therapeutically helps hospitalized children deal with their ailments by concentrating on creating art.
        He has been involved with the organization since 2008 and received their Spirit of Elysium Award in 2012 for his continued support with the charity.
        Wood has supported campaigns such as Keep a Child Alive, ALDO/YouthAIDS, Make a Film Foundation and TOMS Shoes among other civil, political and environmental causes.
        Fellow actor Liev Schreiber, who directed Wood in Everything Is Illuminated, commented that Wood has a "generosity of spirit" and a "sincere goodness as a human being".
        On April 23, 2010, in a charitable gesture, Wood visited the town of Curepto, Chile, one of the hardest hit by the 8.8 magnitude earthquake that struck Chile on February 27, 2010.
        He made personal visits to many of the victims and was accompanied by the First Lady of Chile, Cecilia Morel.[46][47]
        In 2012, Wood sold his $1.75 million home in Santa Monica, California,[48][49] purchased a $698,750 home in Austin, Texas - December 3, 2012.
        Wood is a music lover, owning a collection of roughly 4,000 vinyl records and CDs.
        He has cited The Smashing Pumpkins as his favorite band.[50] Wood has a tattoo of the number nine written in the Sindarin language, which uses the Tengwar script, and in the English language (rather than Quenya as is widely believed), below his waist on the right side.
        It refers to his character as one of the Fellowship of the Ring.
        By coincidence, he had a voice role in a film titled 9.
        The other actors of "The Fellowship" got the same tattoo, with the exception of John Rhys-Davies, whose stunt double got the tattoo instead.[51]
        In May 2006, Autograph Collector Magazine published its list of 10 Best & 10 Worst Hollywood Autograph Signers; Wood was ranked #7 of Best Signers.[52]
        In 2008, during an appearance on Jack Osbourne's show Adrenaline Junkie, he became the first person to cross Victoria Falls on ropes.[53] Wood supported 2016 presidential candidate Bernie Sanders.[54]
        Wood is a fan of the podcast My Dad Wrote a Porno and appeared on an episode of the podcast in 2016.[55]
        In 2016, Wood spoke of his belief that organized child sexual abuse does happen in the Hollywood film industry.[56]"""

    def get_movies_from_text(self):
        for ent in self.nlp(self.text).ents:
            movie = ent
            year = ''
            if ent.label_ != "DATE":
                for i in ent.subtree:
                    if i.pos_ == "NUM":
                        if len(i) == 4:
                            try:
                                if int(i.text) > 1900:
                                    year = i
                                    break
                            except:
                                pass
                        else:
                            if "-" in i.text:
                                print([i for i in ent.subtree])
                                print(i)

            if year:
                self.result.append((movie.text, year.text))

    def compare_result_and_truth(self):
        print("found {} entities with years".format(len(self.result)))
        res = set.intersection(set(self.result), set(self.truth))

        print("matched {} movies with year, out of total {}".format(len(res), len(self.truth)))
        res2 = set.intersection(set([i[0] for i in self.result]), set([i[0] for i in self.truth]))
        print("matched {} movies without year, out of total {}".format(len(res2), len(self.truth)))

        print("mismatched: {}".format(set(self.result) - set(self.truth)))

    def estimate_result(self):
        movies_mentioned = [i for i in self.truth if i[0] in self.text]
        print("There are {} movies mentioned in the text out of {}".format(len(set(movies_mentioned)), len(self.truth)))


if __name__ == "__main__":
    dp = DataProcessor()
    # get_truth()
    # scrape_wikipedia()
    dp.get_movies_from_text()
    dp.estimate_result()
    dp.compare_result_and_truth()