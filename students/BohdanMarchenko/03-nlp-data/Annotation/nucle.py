from bs4 import BeautifulSoup
import statistics

TEAMS = ['a', 'b', 'c']

def agreed_2():
    #at least 2 agreed
    docs = []
    for team in TEAMS:
        with open("conll14st-test-data/alt/alternative-team{}.sgml".format(team), 'r') as fl:
            soup = BeautifulSoup(fl, 'lxml')
            for i in range(1, 51):
                doc = soup.find('doc', {'nid': str(i)})
                if doc:
                    docs += doc.findAll('mistake')
    agreement = (len(docs) - len(set(docs)))/(len(docs)/3)
    print("At least 2 agreed. AGREEMENT RATE: {}%".format(round(agreement * 100), 3))
    # OUTPUT:
    # AGREEMENT RATE: 31%

def all_agreed():
    # all 3  agreed
    docs = []
    agreement = []
    for team in TEAMS:
        with open("conll14st-test-data/alt/alternative-team{}.sgml".format(team), 'r') as fl:
            soup = BeautifulSoup(fl, 'lxml')
            mistakes = []
            for i in range(1, 51):
                doc = soup.find('doc', {'nid': str(i)})
                if doc:
                    mistakes.append(doc.findAll('mistake'))
                else:
                    mistakes.append([])
            docs.append(mistakes)

    for doc in range(50):
        dc_mistakes = [set(docs[team][doc]) for team in range(len(TEAMS))]
        agreement.append(len(set.intersection(*dc_mistakes))/(sum([len(i) for i in dc_mistakes])/3))
    print("All agreed. AGREEMENT RATE: {}%".format(round(statistics.mean(agreement)*100, 2)))
    # OUTPUT:
    #AGREEMENT RATE: 1.72%

if __name__ == "__main__":
    agreed_2()
    all_agreed()

