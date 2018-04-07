#!/usr/bin/env python
import re
from lxml import etree
synonym_re = re.compile("{{sinónimo\|(.*)}}")

def get_synomyms(text):
    res = re.search(synonym_re, text)
    if res:
        return res.groups()[0].split("|")

def process():
    events = ("start", "end")
    context = etree.iterparse("eswiktionary-20180301-pages-meta-current.xml", events=events)
    for action, elem in context:
        if action == "start" and "title" in elem.tag:
            title = elem.text
        if action == "start" and "text" in elem.tag:
            if elem.text and "{sinónimo" in elem.text:
                synonyms = get_synomyms(elem.text)
                if synonyms:
                    print("{}: {}".format(title, ", ".join(synonyms)))
# perspectiva axonométrica: proyección axonométrica
# charnela: bisagra, gozne
# subsidio: ayuda, asistencia, auxilio, subvención
# isotaca: isotaquia, isocinética
# lunar: topo
# descendiente: sucesor
# proporción: armonía, correspondencia
# bagaje: equipaje
# ermitaño: anacoreta, eremita, penitente
# bárbaro: inculto, rústico, bruto, rudo, bestia, burro


if __name__ == "__main__":
    process()