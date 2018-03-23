from bs4 import BeautifulSoup
import statistics

def process():
    TEAMS = ['a', 'b', 'c']
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
        agreement.append(len(set.intersection(*dc_mistakes))/max([len(i) for i in dc_mistakes]))
    print("AGREEMENT RATE: {}%".format(round(statistics.mean(agreement)*100, 2)))
    # OUTPUT:
    #AGREEMENT RATE: 1.19%


if __name__ == "__main__":
    process()

