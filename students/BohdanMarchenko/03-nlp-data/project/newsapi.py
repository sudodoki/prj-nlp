import requests
import urllib.parse
url = ('https://newsapi.org/v2/everything?'
       'q={}&'
       'from=2012-03-29&'
        'to=2018-06-29&'
       'sortBy=popularity&'
       'apiKey=1093c5118ffe41b785e644cdeaf4fd16'.format(urllib.parse.quote_plus('BMW AND "climate change"')))
response = requests.get(url)
js= response.json()
import ipdb; ipdb.set_trace()