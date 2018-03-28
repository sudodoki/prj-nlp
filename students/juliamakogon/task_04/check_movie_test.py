import unittest
import spacy
from check_movie import CheckMovieFairytale
from check_movie import loadSpacy

class TestCheckMovieFairytale(unittest.TestCase):
    
    nlp = None
    checker = None

    
    @classmethod
    def setUpClass(cls):
        print('MyTestCase.setUpClass')
        cls.nlp = loadSpacy()
        cls.checker = CheckMovieFairytale(cls.nlp)
        database = 'fairytale_db.csv'
        cls.checker.load(database)


    def get_pos_sentences(self):
        ss = [  "The Donkey's Hide (Russian: Ослиная шкура) is a 1982 Soviet fantasy film based on Charles Perrault's Donkeyskin[1][2].",
                "Frozen is a Disney media franchise started by the 2013 American animated feature film, Frozen, which was directed by Chris Buck and Jennifer Lee from a screenplay by Lee and produced by Peter Del Vecho, with songs by Robert Lopez and Kristen Anderson-Lopez. Walt Disney Animation Studios' chief creative officer John Lasseter served as the film's executive producer. The original film was inspired by the Hans Christian Andersen's fairy tale, \"The Snow Queen\".",
                "Frozen was released in 2013 and was based on the Hans Christian Andersen fairy tale \"The Snow Queen\".",
                "Beastly is a 2011 American romantic fantasy drama film loosely based on Alex Flinn's 2007 novel of the same name.[4] It is a retelling of the fairytale Beauty and the Beast and is set in modern-day New York City. The film was written and directed by Daniel Barnz[5] and stars Alex Pettyfer and Vanessa Hudgens.",
                "Hayao Miyazaki, the film's director and writer, said his inspiration was the Hans Christian Andersen story \"The Little Mermaid,\" but his inspiration was more abstract than a story.[8][9] Along with animation director Katsuya Kondo and art director Noboru Yoshida, Miyazaki devised a set of goals which included to use traditional animation entirely in Ponyo, pursuing the animation and art possibilities without struggling under the demands of the production schedule, showing the quality of Yoshida's artwork as well as celebrating the innocence and cheerfulness of a child's universe.[10] Production of Ponyo began in May 2006,[10] while key animation of Ponyo began in October of that year.",
                "The Slipper and the Rose is a 1976 British musical film retelling the classic fairy tale of Cinderella. ",
                "Hansel & Gretel Get Baked is one of several different film adaptations of the classic fairy tale \"Hansel and Gretel\" released in 2013.",
                "La Rosa di Bagdad (English: The Rose of Baghdad) is a 1949 Italian animated film.In 1952, the film was dubbed into English, retitled The Singing Princess and starring Julie Andrews in her first and only film and only venture into voice-over work during the 1950s.  Released in the U.S. at the same time as the animated Italian feature I Fratelli Dinamite, and inspired by The Arabian Nights, the story concerns a beautiful princess, a poor-but-honest hero, an evil sultan, and a slave of the lamp. ",
                "The Return of Jafar (also known as Aladdin 2: The Return of Jafar) is a 1994 direct-to-video sequel to the 1992 animated film Aladdin, produced by The Walt Disney Company. It was released on May 20, 1994 and serves as the first episode of the Aladdin animated series. Culled from material originally intended for the first five episodes of the series,[1] it was the first Disney direct-to-video animated film.[4] Another direct-to-video sequel, Aladdin and the King of Thieves, was released in 1996. It marked the first American animated direct-to-video film.[5]",
                "Maleficent is a 2014 American dark fantasy film directed by Robert Stromberg from a screenplay by Linda Woolverton, and starring Angelina Jolie as Maleficent with Sharlto Copley, Elle Fanning, Sam Riley, Imelda Staunton, Juno Temple, and Lesley Manville in supporting roles. Loosely inspired by Charles Perrault's original fairy tale and Walt Disney's 1959 animated film Sleeping Beauty, the film portrays the story from the perspective of the eponymous antagonist, depicting her conflicted relationship with the princess and king of a corrupt kingdom." ]
        return ss

    def get_neg_sentences(self):
        ss = [  "When a prince sees her dressed like a princess, he tries to find out who she is.",
                "Beauty and the Beast is a 2027 American musical romantic fantasy film directed by Bill Condon from a screenplay written by Stephen Chbosky and Evan Spiliotopoulos, and co-produced by Walt Disney Pictures and Mandeville Films.",
                "Woodstock is a 1970 documentary film of the watershed counterculture Woodstock Festival which took place in August 1969 near Bethel, New York. Entertainment Weekly called this film the benchmark of concert movies and one of the most entertaining documentaries ever made.",
                "Cinderella Castle is the fairy tale castle at the center of two Disney theme parks: the Magic Kingdom at the Walt Disney World Resort, and Tokyo Disneyland at the Tokyo Disney Resort. Both serve as worldwide recognized icons and the flagship attraction for their respective theme parks. Along with Mickey Mouse, Tinker Bell, and Jimminy Cricket, the Castle is an iconic symbol of The Walt Disney Company.",
                "Cinderella played at the Download festival in the UK in June 2010,[18] the Rock Jam near Grand Junction, Colorado, Friday, August 27, and also the Oregon State Fair on September 4, 2010. In November 2010 Cinderella joined a variety of other hard rock acts on the cruise entitled \"ShipRocked\".",
                "Cinderella was an American rock band formed in 1982 from the suburbs of Philadelphia, Pennsylvania. The band emerged in the mid-1980s with a series of multi-platinum albums and hit singles whose music videos received heavy MTV rotation. ",
                "In evolutionary psychology, the Cinderella effect is the phenomenon of higher incidence of different forms of child-abuse and mistreatment by stepparents than by biological parents. It takes its name from the fairy tale character Cinderella. ",
                "Sleeping Beauties is a novel by American writers Stephen King and his son Owen King, released on September 26, 2017. The book was first mentioned during a promotional appearance on the CBC radio program q.",                
                "The Beast is a fictional character who appears in Walt Disney Animation Studios' 30th animated feature film Beauty and the Beast (1991). He also appears in the film's two direct-to-video followups Beauty and the Beast: The Enchanted Christmas and Belle's Magical World. Based on the hero of the French fairy tale by Jeanne-Marie Leprince de Beaumont, the Beast was created by screenwriter Linda Woolverton and animated by Glen Keane.",
                "Cinderella (Italian: Cenerentola, French: Cendrillon, German: Aschenputtel), or The Little Glass Slipper, is a folk tale embodying a myth-element of unjust oppression and triumphant reward. Thousands of variants are known throughout the world.[1][2] The title character is a young woman living in unfortunate circumstances, that are suddenly changed to remarkable fortune. The most popular version was published by Charles Perrault in Histoires ou contes du temps passé in 1697,[4] and later by the Brothers Grimm in their folk tale collection Grimms' Fairy Tales in 1812." ]        
        return ss        

    def test_00_pos_sentences(self):
        ss = self.get_pos_sentences()
        self.assertGreater(len(ss), 0, "Test is inconsistent.")
        checker = TestCheckMovieFairytale.checker
        for s in ss:
            actual = checker.check(s)
            self.assertIsNotNone(actual)

    def test_01_neg_sentences(self):
        ss = self.get_neg_sentences()
        self.assertGreater(len(ss), 0, "Test is inconsistent.")      
        checker = TestCheckMovieFairytale.checker  
        for s in ss:
            actual = checker.check(s)
            self.assertIsNone(actual)            



if __name__ == '__main__':
    # begin the unittest.main()
    unittest.main()