import wikipediaapi
from wikipediaapi.wikipedia import WikipediaPage, WikipediaPageSection
import hashlib

from helpers import preprocess

wiki = wikipediaapi.Wikipedia('en')

def add_section(section=None, path=None, content=None, parent="", output={}):
    if isinstance(section,  WikipediaPageSection):
        title = section.title
        text = section.text
        level = section.level
    else:
        title = "summary"
        text = section
        level = 1
    new_path = f"{path}/{title}"
    start = content.index(text)
    end = start + len(text)
    hash_value = hashlib.md5(new_path.encode()).hexdigest()
    output["sections"][hash_value] = {"parent": preprocess(parent),
                          "level": level,
                          "start": start,
                          "end": end,
                          "title": preprocess(title),
                          "path": preprocess(new_path)}
    return output, new_path

def parse_sections(data, content=None, output=None, parent="", path=""):
    if output is None:
        output = {"sections": {}}
    if isinstance(data, WikipediaPage):
        output, _ = add_section(data.summary, path, content, parent, output)
    for s in data.sections:
        output, new_path = add_section(s, path, content, parent, output)
        if s.sections:
            parse_sections(s, parent=s.title, output=output, path=new_path, content=content)
    return output

def get_wiki_json(title):
    page = wiki.page(title)
    output = parse_sections(page, content=page.text)
    output["text"] = preprocess(page.text)
    return output
