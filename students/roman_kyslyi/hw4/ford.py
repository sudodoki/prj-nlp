import re
import requests
from bs4 import BeautifulSoup
import spacy
import difflib

source_page = "http://www.imdb.com/name/nm0000148/"

def is_year(year):
    if len(year) == 4:
        try:
            if int(year.text) > 1900:
                return True
        except:
            pass
    return False


def movie_year(ent, result):
    movie = ent
    if ent.label_ != "DATE":
        for i in ent.subtree:
            if i.pos_ == "NUM":
                if is_year(i) and i.text != ent.text:
                    result.append((movie.text.replace('\n','').strip(), i.text))
                    break



def imdb():
    imdb_list= []
    response = requests.get(source_page)
    soup = BeautifulSoup(response.text, 'html5lib').find("div", {"class": "filmo-category-section"})
    movies = soup.find_all("div", {"class": "filmo-row"})
    for movie in movies:
        year = movie.find("span", {"class": "year_column"}).text.strip()
        name = movie.find("b").text
        imdb_list.append((name, year))
    return imdb_list

list1 = imdb()
list_en = []
list_uk = []
for ent in list1:
    if bool(re.search('[а-яА-Я]', ent[0])): list_uk.append(ent)
    else: list_en.append(ent)

print(len(list_en))
#57
print(len(list_uk))
#17

spacy_nlp = spacy.load("en")
nlp = spacy.load("en")
wiki_page = ('https://uk.wikipedia.org/wiki/%D0%93%D0%B0%D1%80%D1%80%D1%96%D1%81%D0%BE%D0%BD_%D0%A4%D0%BE%D1%80%D0%B4')
response = requests.get(wiki_page)
soup = BeautifulSoup(response.text, 'html5lib')

sd = soup.find("div", {"class": "mw-parser-output"})
text =  " ".join([i.text for i in sd.findAll("p")])
doc = nlp(text)
t = "".join([sent.text for sent in doc.sents])


sd = soup.find("table", {"class": "simple sortable"})
text =  " ".join([i.text for i in sd.findAll("tr")])
doc = nlp(text)

result_uk = []
for ent in doc.ents:
    movie_year(ent,result_uk)
print(result_uk)


wiki_page = ('https://en.wikipedia.org/wiki/Harrison_Ford')
response = requests.get(wiki_page)
soup = BeautifulSoup(response.text, 'html5lib')

sd = soup.find("div", {"class": "mw-parser-output"})
text =  " ".join([i.text for i in sd.findAll("p")])
doc = nlp(text)



result_en = []
for ent in doc.ents:
    movie_year(ent,result_en)


def compare(comp, original):
    true_pos =0
    not_found = len(original) - len(comp)
    true_neg = 0
    for entety in comp:
        for ent in original:
            if entety[1] == ent[1]:
                seq = difflib.SequenceMatcher(None,entety[0],ent[0])
                d = seq.ratio()*100
                if d>50:
                    true_pos +=1
                    break
                else:true_neg +=1
                print(entety)
                print(ent)
    return true_pos, true_neg, not_found



true_pos, true_neg, not_found =compare(result_en,list_en)
print(true_pos, true_neg, not_found)
# 29 44 -8

true_pos, true_neg, not_found =compare(result_uk,list_uk)
print(true_pos, true_neg, not_found)
# 0 12 -15

# З англійською вийшло трохи простіше - з 57 знайдених фільмів збіглося 29, не збіглося 36 i було знайдено 8 нових фільмів в Вікі,
# яких не було в imdb. Помилки в основному відбуваються за рахунок того що деякі назви не витягуються повністю:
# ('K-19', '2002') порівнювалося з ('K-19: The Widowmaker', '2002') i відповідно фільм потрапив в true_neg
# З українською не вийшло зробити порівняння, навіть витягуючи назви з таблиці, а не тексту (що по ідеї мало б бути простіше).
# Основна проблема була з тим що назви витягалися у вигляді the Lost ArkІндіана Джонс, фМалюк з Сан-ФранцискоThe Frisco KidТоммі Лайлард
# тому порівняння українських назв фільмів назвати корректним не можна
#