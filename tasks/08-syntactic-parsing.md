# Синтаксичний аналіз

## I. Покращення парсера залежностей

Візьміть за основу парсер залежностей, побудований на практичному занятті, і зробіть мінімум одну ітерацію для покращення якості.

Варіанти покращення:
1. підберіть кращі ознаки
2. спробуйте інший класифікатор
3. замініть статичний оракул динамічним
4. додайте визначення типів залежностей
5. ваші ідеї

Корисні посилання:
* [UD-корпус для української](https://github.com/UniversalDependencies/UD_Ukrainian-IU/)
* [Зручна бібліотека для роботи з форматом CoNLL](https://github.com/EmilStenstrom/conllu)
* Стаття з блогу Matthew Honnibal - [Parsing English in 500 Lines of Python](https://explosion.ai/blog/parsing-english-in-python)
* Книга про парсери залежностей - [Dependency Parsing by Kübler, McDonald, and Nivre](https://books.google.com.ua/books?id=k3iiup7HB9UC&pg=PA21&hl=uk&source=gbs_toc_r&cad=4#v=onepage&q&f=false)
* Гарний огляд типів парсера залежностей та оракулів - [Improvements in Transition Based Systems for Dependency Parsing](http://paduaresearch.cab.unipd.it/8004/1/Tesi.pdf)

## II. Використання парсера на нових даних

Виберіть кілька випадкових речень українською мовою на побудуйте дерева залежностей для них, використовуючи свій парсер.

Для токенізації можна використати https://github.com/lang-uk/tokenize-uk.

Для частиномовного аналізу можна використати https://github.com/kmike/pymorphy2. Зважте, що частиномовні теги в UD та в pymorphy2 відрізняються, зокрема pymorphy2 не розрізняє типи сполучників. Нижче подаю спосіб вирівняти ці дві нотації:

```python
mapping = {"ADJF": "ADJ", "ADJS": "ADJ", "COMP": "ADJ", "PRTF": "ADJ",
           "PRTS": "ADJ", "GRND": "VERB", "NUMR": "NUM", "ADVB": "ADV",
           "NPRO": "PRON", "PNCT": "PUNCT", "PRED": "ADV", "PREP": "ADP",
           "PRCL": "PART"}

def normalize_pos(word):
    if word.tag.POS == "CONJ":
        if "coord" in word.tag:
            return "CCONJ"
        else:
            return "SCONJ"
    else:
        return mapping.get(word.tag.POS, word.tag.POS)
```

Запишіть ваші спостереження та результати в окремий файл.

Крайній термін: 26.04.2018


