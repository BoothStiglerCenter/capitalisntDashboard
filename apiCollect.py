import pandas as pd
import requests
import json

### READ IN API KEY
api_keys_dict = json.load(open('env_keys.json'))
simplecast_key = api_keys_dict.get('simplecast_key')


auth_headers = {
    'authorization' : 'Bearer {}'.format(simplecast_key)
}


#url = 'https://api.simplecast.com/podcasts'

podcast_id = '912f8fce-96d2-4583-92f5-2279c08e377a'

url = 'https://api.simplecast.com/analytics/episodes?podcast='+podcast_id


response = requests.get(url, headers=auth_headers)
print(response.text)

