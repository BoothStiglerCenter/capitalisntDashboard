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


class MismatchError(Exception):
    """ 
        Exception raised when two values are not the same
        Particularly useful when auditing the length of something
    """
    def __init__(self, lhs, rhs, message='Two values do not match. Try resetting and starting again'):
        self.lhs = lhs
        self.rhs = rhs
        self.message='Two values do not match (LHS: {}   RHS: {})'.format(lhs, rhs)
        super().__init__(self.message)



### GLOBAL VARS OF INTEREST
capitalisnt_podcast_id = '912f8fce-96d2-4583-92f5-2279c08e377a'
today = datetime.now()
files_in_dir = os.listdir()






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

    for filename in files_in_dir:
        if re.search(r'^episodes_core', filename):
            last_collected_datetime_string = re.search(r'\d{4}-\d{2}-\d{2}', filename)[0]
            last_collected_datetime = datetime.strptime(last_collected_datetime_string, '%Y-%m-%d')
 
    try:
       time_since_collection = current_datetime - last_collected_datetime
    except:
        time_since_collection = current_datetime - datetime.strptime('2000-01-01', '%Y-%m-%d')

    eps_response = requests.get(url, headers=auth_headers, params=get_eps_params)
    r = eps_response.json()

    eps_collection = r.get('collection')
    episodes_list = eps_collection

    expected_episodes = r.get('count')
    pages_dict = r.get('pages')
    expected_pages = pages_dict.get('total')
    current_page = pages_dict.get('current')
    next_page_url = pages_dict.get('next').get('href')

    print('Expected # of episode pages: {}'.format(expected_pages))

    if time_since_collection.days < 20:
        # SOME OTHER LOGIC ACCORDING TO READING THE CURRENT DATA FRAME, OVERWRITING AND DROPPING DUPLICATES
        old_df = pd.read_csv()

        new_df = pd.DataFrame.from_dict(episodes_list)
        new_df['season'] = new_df['season'].apply(lambda obj: obj.get('number'))
        new_df['season-ep'] = new_df.apply(lambda row: 'S{}-EP{}'.format(row.season, row.number), axis=1)

        new_df.rename(columns={
            'href' : 'episode_download_href',
            'id' : 'episode_id'
        }, inplace=True)
        new_df.drop(columns=['season', 'number', 'downloads'], inplace=True)

        df = pd.concat([old_df, new_df])
        df = df.drop_duplicates()

        if len(df) != expected_episodes:
            raise MismatchError(len(df), expected_episodes)

        return df
    else:

        ### TODO:  CHANGE THIS TO 'expected_pages' WHEN WE ARE READY TO GO
        while current_page < expected_pages:
            now_current_collection, current_page, next_page_url = getNext(next_page_url)
            print('Just collected {}; about to collect {} out of {} pages'.format(current_page -1, current_page, expected_pages))

            episodes_list += now_current_collection

        print('Finished collection {} out of {} pages'.format(current_page, expected_pages))
        df = pd.DataFrame.from_dict(episodes_list)
        df['season'] = df['season'].apply(lambda obj: obj.get('number'))
        df['season-ep'] = df.apply(lambda row: 'S{}-EP{}'.format(row.season, row.number), axis=1)

        df.rename(columns={
            'href' : 'episode_download_href',
            'id' : 'episode_id'
        }, inplace=True)
        df.drop(columns=['season', 'number', 'downloads'], inplace=True)

        episodes_core_out_path = 'episodes_core-{}.csv'.format(datetime.strftime(current_datetime), '%Y-%m-%d')
        if len(df) != expected_episodes:
            raise MismatchError(len(df), expected_episodes)
        df.to_csv(episodes_core_out_path, index=False, encoding='utf-8')

        return df


x = getAllEpisodes(today)

x