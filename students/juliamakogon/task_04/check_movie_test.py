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
        database = 'cinderella_db.csv'
        cls.checker.load(database)


    def test_00_extractName_NNP_is_movie(self):
        ss = [  "The Donkey's Hide (Russian: Ослиная шкура) is a 1982 Soviet fantasy film based on Charles Perrault's Donkeyskin[1][2].",
                "Language\nEnglish\n\n\nWet Wilderness is a 1976 pornographic horror film directed by Lee Cooper, and produced by Robert Thomas.",
                "Cinderella III: A Twist in Time (released in UK as simply Cinderella: A Twist in Time) is the second direct-to-video sequel to the 1950 Walt Disney Pictures animated classic Cinderella. ",
                "A Cinderella Story: Once Upon a Song is a 2012 teen comedy musical film, directed by Damon Santostefano and starring Lucy Hale, Freddie Stroma, Megan Park, Manu Narayan and Missi Pyle."]
                #"Cinderella, originally released directly to video in 1994, is a 48-minute animated film adapted from the classic fairy tale, \"Cinderella\" by Charles Perrault. The movie was produced by Jetlag Productions and was distributed to DVD in 2002 by GoodTimes Entertainment as part of their \"Collectible Classics\" line."]
                #"Cinderella (a.k.a. The Other Cinderella) is a 1977 American erotic musical comedy film directed by Michael Pataki and starring Cheryl \"Rainbeaux\" Smith, Brett Smiley, and Sy Richardson." ] 
        expected = ['The Donkey\'s Hide', 
                    'Wet Wilderness',
                    'Cinderella III: A Twist in Time',
                    'A Cinderella Story: Once Upon a Song']
        checker = TestCheckMovieFairytale.checker  
        for i in range(len(ss)):
            doc = self.nlp(ss[i])
            actual = checker.extractName_NNP_is_movie(doc)    
            self.assertEqual(actual, expected[i])           
  

    def test_01_extractYear_is_yyyy_movie(self):
        ss = [  "The Donkey's Hide (Russian: Ослиная шкура) is a 1982 Soviet fantasy film based on Charles Perrault's Donkeyskin[1][2].",
                "A Ride for Cinderella is a 1937 Technicolor cartoon sponsored film, and is a sequel to A Coach for Cinderella",
                "Language\nEnglish\n\n\nWet Wilderness is a 1976 pornographic horror film directed by Lee Cooper, and produced by Robert Thomas.",
                "A Cinderella Story: Once Upon a Song is a 2012 teen comedy musical film, directed by Damon Santostefano and starring Lucy Hale, Freddie Stroma, Megan Park, Manu Narayan and Missi Pyle." ]
        expected = ['1982', 
                    '1937',
                    '1976',
                    '2012']
        checker = TestCheckMovieFairytale.checker  
        for i in range(len(ss)):
            doc = self.nlp(ss[i])
            actual = checker.extractYear_is_yyyy_movie(doc)    
            self.assertEqual(actual, expected[i])    


    def test_02_extractBased_film_based_on(self):
        ss = ["The Donkey's Hide (Russian: Ослиная шкура) is a 1982 Soviet fantasy film based on Charles Perrault's Donkeyskin[1][2]."]
        expected = ["Charles Perrault 's Donkeyskin"]
        checker = TestCheckMovieFairytale.checker  
        for i in range(len(ss)):
            doc = self.nlp(ss[i])
            actual = checker.extractBased_film_based_on(doc)    
            self.assertEqual(actual, expected[i])                  


    def test_03__extractBased_Cinderella(self):
        ss = ["A Ride for Cinderella is a 1937 Technicolor cartoon sponsored film, and is a sequel to A Coach for Cinderella" ]
        expected = ["Cinderella"]
        checker = TestCheckMovieFairytale.checker  
        for i in range(len(ss)):
            doc = self.nlp(ss[i])
            actual = checker.extractBased_Cinderella(doc)    
            self.assertEqual(actual, expected[i])   

    # was released 
    #  The film was released on February 6, 2007 and was directed by Frank Nissen and features the voices of Jennifer Hale and Susanne Blakeslee. 

if __name__ == '__main__':
    # begin the unittest.main()
    unittest.main()