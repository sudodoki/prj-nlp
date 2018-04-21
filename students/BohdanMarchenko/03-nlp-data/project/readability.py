import os
import requests
from boilerpipe.extract import Extractor
for filename in os.listdir('scrape_result3'):

url = 'https://www.ft.com/content/0442d5cf-c942-3119-958c-016a8427213b'
extractor = Extractor(extractor='ArticleExtractor', url=url)
extracted_text = extractor.getText()
# response = requests.get('https://seekingalpha.com/article/4132686-exxon-mobil-changes-course-climate-disclosures')
import ipdb; ipdb.set_trace()
# doc = Document(response.text)
