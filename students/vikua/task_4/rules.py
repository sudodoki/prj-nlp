

class Rule(object):
    def apply(self, doc):
        raise ValueError('Not implemented')

    def merge(self, entities, found_actors):
        result = []
        for a in found_actors:
            for e in entities:
                if a in e:
                    result.append(e)
        return result


class ActorPerformanceMentionRule(Rule):
    """ Somebody's performace
    """

    def apply(self, doc):
        entities = [e.text for e in doc.ents if e.label_ in {'PERSON', 'ORG'}]

        found_actors = []
        for d in doc:
            if d.pos_ == 'PROPN' and d.dep_ == 'poss':
                has_part_child = len([c for c in d.children if c.text == "'s"]) == 1
                has_performance_parent = [p for p in d.ancestors if p.lemma_ == 'performance']
                if has_part_child and has_performance_parent:
                    found_actors.append(d.text)

        result = self.merge(entities, found_actors)
        return [r.replace("'s", '') if "'s" in r else r for r in result]


class ActorCastRule(Rule):
    """ Somebody was cast
    """

    def apply(self, doc):
        entities = [e.text for e in doc.ents if e.label_ in {'PERSON', 'ORG'}]

        found_actors = []
        for d in doc:
            if d.pos_ == 'PROPN' and d.dep_ == 'nsubjpass':
                has_cast_parent = [p for p in d.ancestors if p.lemma_ == 'cast']
                if has_cast_parent:
                    found_actors.append(d.text)

        return self.merge(entities, found_actors)


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

    def apply(self, doc):
        entities = [e.text for e in doc.ents if e.label_ in {'PERSON', 'ORG'}]

        found = []
        for d in doc:
            if d.pos_ == 'VERB' and d.lemma_ == 'star':
                for c in d.children:
                    if c.pos_ == 'PROPN' and c.dep_ == 'dobj':
                        found.append(c.text)
                        found = self.find_recursive(c, found)

        return self.merge(entities, found)