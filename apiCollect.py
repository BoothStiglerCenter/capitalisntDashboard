import pandas as pd
import requests
import os
import re
import time
import json
from datetime import datetime


### READ IN API KEY
api_keys_dict = json.load(open('env_keys.json'))
simplecast_key = api_keys_dict.get('simplecast_key')
auth_headers = {
    'authorization' : 'Bearer {}'.format(simplecast_key)
}

### GLOBAL VARS OF INTEREST
capitalisnt_podcast_id = '912f8fce-96d2-4583-92f5-2279c08e377a'
today = datetime.now().date()
files_in_dir = os.listdir()
date_format = '%Y-%m-%d'
date_pattern  = r'\d{4}-\d{2}-\d{2}'
default_date = datetime.strptime('2000-01-01', date_format).date()

class MismatchError(Exception):
    """ 
        Exception raised when two values are not the same
        Particularly useful when auditing the length of something
    """
    def __init__(self, lhs, rhs, message=""):
        self.lhs = lhs
        self.rhs = rhs
        self.message='Two values do not match (LHS: {}   RHS: {})'.format(lhs, rhs)
        super().__init__(self.message)


### Identifies whether a certain type of .csv has already been collected (ie is in this folder somewhere)
def identifyExistingCollection(current_datetime, type_csv_search_pattern):

    print('Trying to identify previous collection exists for: {}'.format(type_csv_search_pattern))
    # Establishes patterns so that this function is generalizable to lots of types of .csvs
    patterns_dict = {
        'episodes_core' : r'^episodes_core',
        'episodes_downloads' : r'^episodes_downloads',
        'keywords' : r'^episodes_keywords',
    }

    
    pattern = patterns_dict.get(type_csv_search_pattern)
    # Sets the default last-collected date. If no files match the desired pattern, this is the date that's returned.
    last_collected_datetime = default_date


    for filename in files_in_dir:
        if re.search(pattern, filename):
            last_collected_datetime_string = re.search(date_pattern, filename)[0]
            last_collected_datetime = datetime.strptime(last_collected_datetime_string, date_format).date()
            break

    # If we've done some kind of collection before, load in the old one and pass the date it was last collected back to the function that called this. Otherwise, pass back the default date (2000/01/01) and note that we have never collected this type of df before.
    if default_date < last_collected_datetime: 
        for i, filename in enumerate(files_in_dir):
            if re.search(pattern, filename):
                old_df_path = files_in_dir[i]
                break
        collection = pd.read_csv(old_df_path)
    elif last_collected_datetime == default_date:
        collection = 'not previously collected'

    return collection, last_collected_datetime


def getNext(next_url):
    next_response = requests.get(next_url, headers=auth_headers)
    next_response_json = next_response.json()

    now_current_collection = next_response_json.get('collection')
    now_current_page_num = next_response_json.get('pages').get('current')
    now_next_page_url = next_response_json.get('pages').get('next').get('href')
    
    return now_current_collection, now_current_page_num, now_next_page_url  

def getAllEpisodes(current_datetime):
    get_eps_params = {
        'podcast' : capitalisnt_podcast_id
    }
    url = 'https://api.simplecast.com/analytics/episodes'


    old_df, last_collected_datetime = identifyExistingCollection(current_datetime, 'episodes_core')

    if type(old_df) == str:
        old_df = pd.DataFrame()
 
    time_since_collection = current_datetime - last_collected_datetime


    eps_response = requests.get(url, headers=auth_headers, params=get_eps_params)
    r = eps_response.json()

    eps_collection = r.get('collection')
    episodes_list = eps_collection

    expected_episodes = r.get('count')
    pages_dict = r.get('pages')
    expected_pages = pages_dict.get('total')
    current_page = pages_dict.get('current')
    next_page_url = pages_dict.get('next').get('href')

    

    # If we have collected within the past 50 days, only call the API once to get the most recent episode IDs. Otherwise, we can't really count on how many observations we have so collect every observation that you can get out of this API endpoint 'analytics/episodes'
    if time_since_collection.days < 50:
        print('Last episodes_core collection was within 50 days. Calling limited API')

        new_df = pd.DataFrame.from_dict(episodes_list)
        new_df['season'] = new_df['season'].apply(lambda obj: obj.get('number'))
        new_df['season-ep'] = new_df.apply(lambda row: 'S{}-EP{}'.format(int(row.season), int(row.number)), axis=1)

        new_df.rename(columns={
            'href' : 'episode_download_href',
            'id' : 'episode_id'
        }, inplace=True)
        new_df.drop(columns=['season', 'number', 'downloads'], inplace=True)

        df = pd.concat([old_df, new_df])
        df = df.drop_duplicates(subset=['episode_download_href'])

        if len(df) != expected_episodes:
            raise MismatchError(len(df), expected_episodes)

        return df
    
    else:
        # The API only returns 5 episodes at a time so we need to keep calling until we get all 'expected_pages' pages. 
        print('Last episodes_core collection was more than 50 days ago. Collecting all.')
        print('Expected # of episode pages: {}'.format(expected_pages))
        while current_page < expected_pages:
            now_current_collection, current_page, next_page_url = getNext(next_page_url)
            print('Just collected {}; about to collect {} out of {} pages'.format(current_page -1, current_page, expected_pages))

            episodes_list += now_current_collection

        print('Finished collection {} out of {} pages'.format(current_page, expected_pages))
        df = pd.DataFrame.from_dict(episodes_list)
        df['season'] = df['season'].apply(lambda obj: obj.get('number'))
        df['season-ep'] = df.apply(lambda row: 'S{}-EP{}'.format(int(row.season), int(row.number)), axis=1)

        df.rename(columns={
            'href' : 'episode_download_href',
            'id' : 'episode_id'
        }, inplace=True)
        df.drop(columns=['season', 'number', 'downloads'], inplace=True)

        episodes_core_out_path = 'episodes_core-{}.csv'.format(datetime.strftime(current_datetime), date_format)
        if len(df) != expected_episodes:
            raise MismatchError(len(df), expected_episodes)
        df.to_csv(episodes_core_out_path, index=False, encoding='utf-8')

        return df


def getEpDownloads(current_datetime):
    get_downloads_params = {
        'interval' : 'day',
    }

    eps_df, last_collected_datetime = identifyExistingCollection(current_datetime, 'episodes_core')

    if type(eps_df) == str:
        print('Could not find a episodes_core .csv. Trying to redownload those.')
        eps_df = getAllEpisodes(current_datetime)
    elif eps_df < current_datetime:
        print('The episodes_core .csv may be out of date. Calling that API again.')
        eps_df = getAllEpisodes(current_datetime)
    eps_df = eps_df[['episode_download_href', 'title', 'episode_id', 'season-ep']]

    eps_downloads_df, downloads_last_collected_datetime = identifyExistingCollection(current_datetime, 'episodes_downloads')

    # If we've never collected episode downloads before, we got a string back from identifyExistingCollection() and we nee to initialize a df before downloading everything.
    if type(eps_downloads_df) == str:
        print('Could not find a episodes_downloads .csv. Going to begin downloading all')
        eps_downloads_df = pd.DataFrame()
    
    # If we have collected episode downloads before we only want to download new dates (we pass this this to the API)
    if default_date < downloads_last_collected_datetime:
        print('Found an old episodes_downloads .csv dated to {}. Only calling API for more recent download data.'.format(downloads_last_collected_datetime))
        get_downloads_params['start_date'] = downloads_last_collected_datetime.isoformat()  

    for i, obs in eps_df.iterrows():

        episode_downloads_url = obs.episode_download_href
        episode_id = obs.episode_id
        episode_title = obs.title
        print(episode_title)

        response = requests.get(episode_downloads_url,
            headers = auth_headers,
            params = get_downloads_params)

        downloads = response.json().get('by_interval')
        downloads_df = pd.DataFrame.from_dict(downloads)
        downloads_df['episode_id'] = episode_id
        downloads_df['title'] = episode_title

        eps_downloads_df = pd.concat([eps_downloads_df, downloads_df], ignore_index=True)

    eps_downloads_df = eps_downloads_df.drop_duplicates(subset=['episode_id', 'interval'])
    print('trying to get new eps downloads')

    eps_downloads_df.to_csv('episodes_downloads-{}.csv'.format(today), encoding='utf-8')

    return eps_downloads_df

def getKeyWords(current_datetime):
    eps_df, last_collected_datetime = identifyExistingCollection(current_datetime, 'episodes_core')

    if type(eps_df) == str:
        print('Could not find a episodes_core.csv. Trying to redownload those')
        eps_df = getAllEpisodes(current_datetime)
    elif last_collected_datetime < current_datetime:
        print('The episodes_core .csv may be out of date. Calling that API again')
        eps_df = getAllEpisodes(current_datetime)
    eps_df = eps_df[['episode_download_href', 'title', 'episode_id', 'season-ep']]

    keywords_df, keywords_last_collected_datetime = identifyExistingCollection(current_datetime, 'keywords')
    
    if type(keywords_df) == str:
        print('Could not find a keywords .csv. Going to begin downloading all')
        keywords_df = pd.DataFrame()
        keywords_df['episode_id'] = None
    
    if default_date < keywords_last_collected_datetime: 
        print('Found an old keywords .csv dated to {}. Only calling API for more recent download_data'.format(keywords_last_collected_datetime))

    all_eps_episode_id_set = set(eps_df['episode_id'].unique().tolist())
    existing_keywords_episode_id_set = set(keywords_df['episode_id'].unique().tolist())

    keywords_to_collect_episode_id_set = all_eps_episode_id_set - existing_keywords_episode_id_set

    for episode_id in keywords_to_collect_episode_id_set:
        url = 'https://api.simplecast.com/episodes/{}/keywords'.format(episode_id)

        response = requests.get(url, headers=auth_headers)
        keywords = response.json().get('collection')
        keywords_temp_df = pd.DataFrame.from_dict(keywords)
        keywords_temp_df['episode_id'] = episode_id


        keywords_df = pd.concat([keywords_df, keywords_temp_df], ignore_index=True)
        
    keywords_df = keywords_df.drop_duplicates(subset=['episode_id', 'value'])
    keywords_df.to_csv('episodes_keywords-{}.csv'.format(today), encoding='utf-8')
    return keywords_df


# getEpDownloads(today)
getKeyWords(today)
