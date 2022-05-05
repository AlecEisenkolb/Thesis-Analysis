# Twitter API
# Author: Alec Eisenkolb
# Date: 04.05.2022

# load libraries
from cmath import nan
import pandas as pd
import tweepy
import yaml
import os
import time

# define function to load YAML files
def yaml_import(file_name=str):
    """
    This function loads a YAML file. \n\n
    Inputs: 
    - file_name: full path of file, encoded as string. \n\n
    Output:
    - Returns the YAML file accessible in the python environment.
    """
    # use path to load yaml file
    with open(file_name, "r") as path:
        config = yaml.load(path, Loader=yaml.FullLoader)
    # return data
    return config

# Define file_name of config file and import data
file_name = f"{os.path.dirname(os.path.realpath(__file__))}/config.yml"
config = yaml_import(file_name)

# import twitter IDs dataset
PATH = r'/Users/alec/Dropbox/HSG MEcon/Master Thesis/Thesis Analysis/Clean Data/Twitter/twitter_ids.csv'
twitter_ids = pd.read_csv(PATH)

# read Twitter API values for authentication
api_key = config["user_authentication"]["API_Key"]
api_key_secret = config["user_authentication"]["API_Key_Secret"]
api_bearer_token = config["user_authentication"]["Bearer_Token"]
access_token = config["user_authentication"]["Access_Token"]
access_token_secret = config["user_authentication"]["Access_Token_Secret"]

# define function to scrape tweets from user IDs
def get_user_tweets(bearer_token=str, start_date=str, end_date=str, user_IDs=list):
    """
    A function to initiate the client and scrape historical tweets from a list of users. \n\n
    Inputs: 
    - bearer_token: a token to initialize the Twitter API v2 client (must have Academic Research Product Track access) (string).
    - start_date: the earliest date for the tweets to be scraped, in the format: YYYY-MM-DDTHH:MM:SSZ (string).
    - end_date: the latest date for the tweets to be scraped, in the format: YYYY-MM-DDTHH:MM:SSZ (string).
    - user_IDs: a list of Twitter user IDs for which tweets should be scraped. \n\n
    Output:
    - Returns a dataframe containing all the information provided by the Twitter API. 
    """
    # initiate the Twitter API v2 client
    client = tweepy.Client(bearer_token=bearer_token, wait_on_rate_limit=True)

    # prepare data object for tweet observations
    column_names = ["User ID", "Tweet ID", "Date", "Tweet Text", "Language", "Likes", 
					"Replies", "Retweets", "Quotes", "If Reply", "Type", "Parent Tweet ID"]
    tweet_data = []

    # initiate a for-loop over all user IDs
    for userID in user_IDs:

        # define query for tweet search function
        query = f"from:{userID}"

        # include a print statement for visualization of API process on console
        print(f"API getting tweets from user: {user_IDs.index(userID) + 1} from total of {len(user_IDs)} users.")

        # make request to Twitter API
        tweets = tweepy.Paginator(client.search_all_tweets, query=query,
                                  tweet_fields=["public_metrics", "lang", "created_at", "in_reply_to_user_id", "referenced_tweets"],
                                  start_time=start_date, end_time=end_date,
                                  max_results=500).flatten()

        # create a dataset from the tweets data
        for tweet in tweets:
            # check whether referenced_tweets object has entries, if not we set these equal to nan
            if tweet.referenced_tweets is None:
                ref_tweet_type = nan
                ref_tweet_id = nan
            else: 
                ref_tweet_type = str(tweet.referenced_tweets[0]["type"])
                ref_tweet_id = str(tweet.referenced_tweets[0]["id"])

			# append data for tweet to tweet_data object
            tweet_data.append([str(userID), str(tweet.id), tweet.created_at, tweet.text, tweet.lang, tweet.public_metrics["like_count"],
                           	   tweet.public_metrics["reply_count"], tweet.public_metrics["retweet_count"],
                           	   tweet.public_metrics["quote_count"], str(tweet.in_reply_to_user_id), ref_tweet_type, ref_tweet_id])

        # include a print statement for visualization of num. of tweets collected
        print(f"No. of tweets collected: {len(tweet_data)} \n")

        # add a break of above one second to avoid rate limit of one request / one second.
        time.sleep(1.5)

    # create dataframe from tweet_data object
    df = pd.DataFrame(tweet_data, columns=column_names)

    # return dataframe
    return df

# define function to obtain tweet replies from list of tweet IDs
def get_tweet_replies(bearer_token=str, start_date=str, end_date=str, tweet_IDs=list):
    """
	Function using Twitter API v2 to obtain all replies for a given tweet. \n\n
	Inputs:
	- bearer_token: a token to initialize the Twitter API v2 client (string).
    - start_date: the earliest date for the tweets to be scraped, in the format: YYYY-MM-DDTHH:MM:SSZ (string).
    - end_date: the latest date for the tweets to be scraped, in the format: YYYY-MM-DDTHH:MM:SSZ (string). 
    - tweet_IDs: a list of tweet IDs for which all replies should be scraped. \n\n
	Output:
	- Returns a dataframe containing tweet IDs as row identifiers and respective replies as columns.
	"""
    # open connection to Twitter API client
    client = tweepy.Client(bearer_token=bearer_token, wait_on_rate_limit=True)

# # # # # Twitter API: web scraping tweets from user IDs # # # # #

# get twitter user IDs as a list
twitterIDs_list = twitter_ids["user_id"].tolist()

# specify start and end time of tweet collection
start_time = "2021-07-01T00:00:00Z"
end_time = "2021-10-10T00:00:00Z"

# apply function to retrieve tweets by user ID
# df_tweets = get_user_tweets(bearer_token = api_bearer_token, start_date = start_time, end_date = end_time, user_IDs = twitterIDs_list)

# save dataframe to path
EXPORT_PATH = r'/Users/alec/Dropbox/HSG MEcon/Master Thesis/Thesis Analysis/Raw Data/Twitter API/'
# df_tweets.to_csv(f"{EXPORT_PATH}/tweets.csv")

# # # # # Twitter API: run algorithm to check users during rate limit pauses above # # # # #

# get user IDs for 4 specific users in which algorithm was paused due to Twitter's rate limits being reached
indices = [276, 555, 822, 1074] # manually checked which indices of IDs were halted during Twitter API rate limits
check_userID = [twitterIDs_list[i] for i in indices]
print(check_userID)
# obtain tweets by these specific users
#df_tweetcheck = get_user_tweets(bearer_token = api_bearer_token, start_date = start_time, end_date = end_time, user_IDs = check_userID)

# save dataframe
#df_tweetcheck.to_csv(f"{EXPORT_PATH}/tweet_check.csv")

### Step 3: run the algorithm again for those user IDs which did not get any tweets before (due to rate limit being reached and potentially messing these up)

### Step 4: run algorithm to get full tweet texts of all parent tweets. This is important if we want to do NLP on the actual tweets of the users, as RT are concated at a certain length!

### Step 5: run algorithm to get replies of all tweets!


