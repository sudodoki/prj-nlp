import re

class Rule(object):

    def merge(self, entities, found_actors):
        entities = [x for x in entities if len(x.split()) >= 2 and len(x.split()) <= 3]
        result = []
        for a in found_actors:
            for e in entities:
                if a in e:
                    result.append(e)
        return result
    
    def apply(self, sentences):
        all_found = set()
        all_entities = set()
        for doc in sentences:
            found, entities = self.apply_to_one(doc)
            all_found = all_found.union(found)
            all_entities = all_entities.union(entities)
        
        result = self.merge(all_entities, all_found)
        return [r.replace("'s", '') if "'s" in r else r for r in result]
    

class ActorPerformanceMentionRule(Rule):
    """ Somebody's performace
    """
    
    def apply_to_one(self, doc):
        entities = [e.text for e in doc.ents if e.label_ in {'PERSON', 'ORG'}]

        found_actors = []
        for d in doc:
            if d.pos_ == 'PROPN' and d.dep_ == 'poss':
                has_part_child = len([c for c in d.children if c.text == "'s"]) == 1
                has_performance_parent = [p for p in d.ancestors if p.lemma_ == 'performance']
                if has_part_child and has_performance_parent:
                    found_actors.append(d.text)

        return found_actors, entities


class ActorCastRule(Rule):
    """ Somebody was cast
    """

    def apply_to_one(self, doc):
        entities = [e.text for e in doc.ents if e.label_ in {'PERSON', 'ORG'}]

        found_actors = []
        for d in doc:
            if d.pos_ == 'PROPN' and d.dep_ == 'nsubjpass':
                has_cast_parent = [p for p in d.ancestors if p.lemma_ == 'cast']
                if has_cast_parent:
                    found_actors.append(d.text)

        return found_actors, entities


class FilmStartsRule(Rule):
    """ It[movie] stars somebody
    """

    def find_recursive(self, token, found):
        conj = [x for x in token.children if x.dep_ == 'conj']
        if conj:
            found = found + [x.text for x in conj]
            for c in conj:
                return self.find_recursive(c, found)
        else:
            return found

    def apply_to_one(self, doc):
        entities = [e.text for e in doc.ents if e.label_ in {'PERSON', 'ORG'}]

        found = []
        for d in doc:
            if d.pos_ == 'VERB' and d.lemma_ == 'star':
                for c in d.children:
                    if c.pos_ == 'PROPN' and c.dep_ == 'dobj':
                        found.append(c.text)
                        found = self.find_recursive(c, found)

        return found, entities
    
# added later

class FilmStarsRuleAsNoun(FilmStartsRule):
    """ Movie stars (noun)
    """
    
    def apply_to_one(self, doc): 
        entities = [e.text for e in doc.ents if e.label_ in {'PERSON', 'ORG'}]
        
        found = []
        for d in doc: 
            if d.pos_ == 'NOUN' and d.lemma_ == 'star': 
                for c in d.children: 
                    if c.pos_ == 'PROPN' and c.dep_ == 'appos': 
                        found.append(c.text)
                        found = self.find_recursive(c, found)
        return found, entities


class IncludingRule(FilmStartsRule):
    """ Move stars a lot of people including somebody 
    """
    
    def apply_to_one(self, doc): 
        entities = [e.text for e in doc.ents if e.label_ in {'PERSON', 'ORG'}]
        
        found = []
        for d in doc: 
            if d.pos_ == 'VERB' and d.lemma_ == 'include': 
                for c in d.children: 
                    if c.pos_ == 'PROPN' and c.dep_ == 'pobj' and c.text.lower() not in {'screenplay', 'film'}: 
                        found.append(c.text)
                        found = self.find_recursive(c, found)
        return found, entities

    
class IncludesRuleDObjWithCompount(FilmStartsRule):
    """ Move stars a lot of people including somebody 
    """
    
    def apply_to_one(self, doc): 
        entities = [e.text for e in doc.ents if e.label_ in {'PERSON', 'ORG'}]
        
        found = []
        for d in doc: 
            if d.pos_ == 'VERB' and d.lemma_ == 'include': 
                for c in d.children: 
                    if c.pos_ == 'PROPN' and c.dep_ == 'dobj' and c.text.lower() not in {'screenplay', 'film'}: 
                        found.append(c.text)
                        found = self.find_recursive(c, found)
        return found, entities
    

class PlayedByRule(Rule): 
    
    pattern = '.+played by\s(\w+\s\w+).+'
    
    def apply_to_one(self, doc): 
        entities = [e.text for e in doc.ents if e.label_ in {'PERSON', 'ORG'}]
        name = re.search(self.pattern, doc.text)
        if name: 
            return [name.group(1)], entities
        else: 
            return [], entities

        
class FilmStartsWithRule(Rule):
    """ It[movie] stars somebody
    """

    def find_recursive(self, token, found):
        conj = [x for x in token.children if x.dep_ == 'conj']
        if conj:
            found = found + [x.text for x in conj]
            for c in conj:
                return self.find_recursive(c, found)
        else:
            return found

    def apply_to_one(self, doc):
        entities = [e.text for e in doc.ents if e.label_ in {'PERSON', 'ORG'}]

        found = []
        for d in doc:
            if d.pos_ == 'VERB' and d.lemma_ == 'star':
                for c in d.children:
                    if c.text == 'with' and c.dep_ == 'prep':
                        for sc in c.children: 
                            if sc.pos_ == 'PROPN' and sc.dep_ == 'pobj':
                                found.append(sc.text)
                                found = self.find_recursive(sc, found)

        return found, entities

class AnsembleOfCastOfRule(FilmStartsRule): 
    
    def apply_to_one(self, doc): 
        entities = [e.text for e in doc.ents if e.label_ in {'PERSON', 'ORG'}]
        
        found = []
        for d in doc: 
            if d.pos_ == 'NOUN' and d.lemma_ == 'cast': 
                for c in d.children: 
                    if c.pos_ == 'ADP' and c.dep_ == 'prep' and c.text.lower() == 'of': 
                        for sc in c.children:
                            if sc.pos_ == 'PROPN' and sc.dep_ == 'pobj':
                                found.append(sc.text)
                                found = self.find_recursive(sc, found)
        return found, entities

class InBrackets(Rule): 
    
    pattern = '\((.*?)\)'
    
    def apply_to_one(self, doc): 
        entities = [e.text for e in doc.ents if e.label_ in {'PERSON', 'ORG'}]

        found = [r for r in re.findall(self.pattern, doc.text)]
        return found, entities
    

class RootPropNRule(FilmStartsRule): 
    
    def apply_to_one(self, doc): 
        entities = [e.text for e in doc.ents if e.label_ in {'PERSON', 'ORG'}]
        
        found = []
        for d in doc: 
            if d.dep_ == 'ROOT' and d.pos_ == 'PROPN': 
                found.append(d.text)
                found = self.find_recursive(d, found)
        return found, entities



