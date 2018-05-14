subscription_key = "85837ebee5d4472a9fde9b65dd268a71"
search_url = "https://api.cognitive.microsoft.com/bing/v7.0/search"
import requests
search_term = "BMW 'climate change'"

headers = {"Ocp-Apim-Subscription-Key" : subscription_key}
params  = {"q": search_term, "textDecorations":True, "textFormat":"HTML"}
response = requests.get(search_url, headers=headers, params=params)
response.raise_for_status()
search_results = response.json()
import ipdb; ipdb.set_trace()