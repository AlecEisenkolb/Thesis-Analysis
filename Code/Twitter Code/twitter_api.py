# Twitter API
# Author: Alec Eisenkolb
# Date: 10.05.2022

# load libraries
from cmath import nan
import pandas as pd
import tweepy
import yaml
import os
import time

# Define variable "TASK", which determines the functions of the script that will be executed.
# This will allow the script to selectively scrape certain tweets/retweets/replies and not the entire frame.
# Select from: 
# "Testing" = used to test certain parts of script (debugging)
# "Scrape Tweets" = scrapes tweets of all political candidates between specified time period (time: 1 hr)
# "Check Users" = used to check the correct scraping of tweets for specified users
# "Scrape Retweets" = scrapes those tweets of political candidates again that are re-tweets, as RT are not fully compiled when scraped directly (time: 45 min)
# "Scrape Replies" = scrapes all replies to tweets by political candidates (time: 50 hrs)
# or "ALL" if all functions should be executed.
TASK = "Testing"

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
def get_tweet_replies(tweet_dataframe, bearer_token=str, start_date=str, end_date=str):
    """
	Function using Twitter API v2 to obtain all replies for a given tweet. \n\n
	Inputs:
	- bearer_token: a token to initialize the Twitter API v2 client (string).
    - start_date: the earliest date for the tweets to be scraped, in the format: YYYY-MM-DDTHH:MM:SSZ (string).
    - end_date: the latest date for the tweets to be scraped, in the format: YYYY-MM-DDTHH:MM:SSZ (string).
    - tweet_dataframe: a dataframe including Twitter user IDs and tweet IDs per User. \n\n
	Output:
	- Returns a dataframe containing tweet IDs as row identifiers and respective replies as columns.
	"""
    # open connection to Twitter API client
    client = tweepy.Client(bearer_token=bearer_token, wait_on_rate_limit=True)

    # define data for storage of replies
    column_names = ["Parent User ID", "Parent Tweet ID", "Reply ID", "Reply Text", "Reply Lang"]
    reply_data = []

    ##### The commented code chunk below may be used to get the entire conversations of replies, including replies to replies of the politicians tweet (DOESNT WORK YET)
    # loop over each Tweet ID
    # for id in tweet_IDs:
    #     # Visualize the process of API in console
    #     print(f"API getting replies for tweet no. {tweet_IDs.index(id)+1} out of {len(tweet_IDs)} tweets")
        
    #     # define query for API
    #     query = f"conversation_id:{id}"

    #     # use APIs to get replies
    #     replies = tweepy.Paginator(client.search_all_tweets, query=query,
    #                                tweet_fields=["public_metrics", "lang"],
    #                                max_results=500).flatten()
        
    #     # loop over all replies for a given conversation id (Tweet ID)
    #     for reply in replies:
    #         # append data to data object reply_data
    #         reply_data.append([id, reply.id, reply.text, reply.lang])

    #     # Visualize the amount of tweet replies collected
    #     print(f"No. of replies collected: {len(reply_data)} \n")
        
    #     # sleep for 1.5 seconds to respect the rate limits of one request / one second
    #     time.sleep(1.5)

    users_all = list(tweet_dataframe["User ID"].unique())

    # loop over Twitter user IDs
    for userid in users_all:

        # get all tweet IDs available for this user ID
        tweets_all = list(tweet_dataframe[tweet_dataframe.loc[:, "User ID"] == userid]["Tweet ID"])
        tweets_list = [tweets_all[i] for i in range(0, len(tweets_all))]

        # Visualize the process of API in console
        print(f"API getting replies for user no. {users_all.index(userid)+1} out of {len(users_all)} users")
        sum_replies = tweet_dataframe[tweet_dataframe.loc[:, "User ID"] == userid]["Replies"].sum()
        print(f"Possible replies to scrape from this user: {sum_replies} replies \n")

        # define query for API
        query = f"to:{userid}"
        
        # use APIs to get replies
        replies = tweepy.Paginator(client.search_all_tweets, query=query,
                                   start_time=start_date, end_time=end_date,
                                   tweet_fields=["public_metrics", "lang", "in_reply_to_user_id", "referenced_tweets"],
                                   max_results=500).flatten()

        # loop over replies
        for reply in replies:

            # check if tweet is reply to specific tweet 
            if hasattr(reply, 'referenced_tweets'):
                if reply.referenced_tweets is None:
                    continue 
                # if tweet is a reply to any tweet ID that we have scraped of this user, we add it into our reply_data object
                elif (str(reply.referenced_tweets[0]["id"]) in tweets_list):
                    reply_data.append([reply.in_reply_to_user_id, str(reply.referenced_tweets[0]["id"]), reply.id, reply.text, reply.lang])

        # sleep for some time to allow the rate limit not to be exceeded
        time.sleep(1.5)

    # create dataframe object from data
    df = pd.DataFrame(reply_data, columns=column_names)

    # return datafram
    return df

# define function to get tweet information based on tweet unique identifier (single IDs)
def get_tweet_by_ID(bearer_token=str, tweet_IDs=list):
    """
    Function that returns ID and text of tweet by ID. This scrapes each tweet separately, hence takes longer
    than the next function in the script. This function should be used for # of IDs less than 900, as otherwise
    the Twitter API rate limits are too costly. \n
    Inputs:
    - bearer_token: Twitter API bearer token used for authentication (string).
    - tweet_ID: a list of tweet IDs for which text will be scraped. \n
    Outputs:
    - Dataframe object with tweet IDs and Text of the tweet.
    """
    # open connection to Twitter API client
    client = tweepy.Client(bearer_token=bearer_token, wait_on_rate_limit=True)

    # define data object to store tweet data in
    column_names = ["Tweet ID", "Tweet Text"]
    tweet_data = []

    # loop over each list of 100 IDs in the object ID_lists
    for tweet_id in tweet_IDs:

        # print statement to visualize the process of the API
        print(f"API retrieving tweet no. {tweet_IDs.index(tweet_id)+1} from total lists {len(tweet_IDs)}")

        # use Tweepy function with tweet IDs
        tweet = client.get_tweet(id=tweet_id)

        # append data to tweet_data
        tweet_data.append([tweet.data.id, tweet.data.text])
    
    # create dataframe from tweet_data object
    df = pd.DataFrame(tweet_data, columns=column_names)

    # return dataframe
    return df

# define function to get tweet information based on tweet unique identifier (multiple IDs at once)
def get_tweets_IDs(tweet_IDs, bearer_token=str):
    """
    Function that returns ID and text of tweets by list of IDs. This can scrape tweets in chunks of 100,
    hence it is faster for # of tweet IDs that well exceed the 1000s. \n
    Inputs:
    - bearer_token: Twitter API bearer token used for authentication (string).
    - tweet_IDs: a list of tweet IDs for which text will be scraped. \n
    Outputs:
    - Dataframe object with tweet IDs and Text of the tweet.
    """
    # open connection to Twitter API client
    client = tweepy.Client(bearer_token=bearer_token, wait_on_rate_limit=True)

    # define data object to store tweet data in
    column_names = ["Tweet ID", "Tweet Text"]
    tweet_data = []

    # partition the list into subsets of 100, as this is the maximum IDs per request to the Twitter API
    n = 100
    id_sublist = [tweet_IDs[i * n:(i + 1) * n] for i in range((len(tweet_IDs) + n - 1) // n)]

    # start indexation for print-statement within loop
    i = 1
    # loop over all partitions of the list
    for ids in id_sublist:
        # print statement to visualize process of API
        print(f"API getting tweet list no. {i} of total {len(id_sublist)}")

        tweets = client.get_tweets(ids=ids, tweet_fields=["public_metrics"])

        for tweet in tweets.data:
            # append data
            tweet_data.append([tweet.id, tweet.text])

        # continue indexation
        i += 1
    
    # create dataframe from tweet_data object
    df = pd.DataFrame(tweet_data, columns=column_names)

    # return dataframe
    return df

# # # # # Import config file and Twitter API authentication tokens # # # # # 

if __name__ == "__main__":
    # define path for data and export
    PATH = r'/Users/alec/Dropbox/HSG MEcon/Master Thesis/Thesis Analysis'
    EXPORT_PATH = r'/Users/alec/Dropbox/HSG MEcon/Master Thesis/Thesis Analysis/Raw Data/Twitter API/'

    # Define file_name of config file and import data
    file_name = f"{os.path.dirname(os.path.realpath(__file__))}/config.yml"
    config = yaml_import(file_name)

    # read Twitter API values for authentication
    api_key = config["user_authentication"]["API_Key"]
    api_key_secret = config["user_authentication"]["API_Key_Secret"]
    api_bearer_token = config["user_authentication"]["Bearer_Token"]
    access_token = config["user_authentication"]["Access_Token"]
    access_token_secret = config["user_authentication"]["Access_Token_Secret"]

# # # # # Twitter API: web scraping tweets from user IDs # # # # #

    if (TASK == "Scrape Tweets" or TASK == "ALL"):
        # import twitter IDs dataset
        twitter_ids = pd.read_csv(f"{PATH}/Clean Data/Twitter/twitter_ids.csv")

        # get twitter user IDs as a list
        twitterIDs_list = twitter_ids["user_id"].tolist()

        # specify start and end time of tweet collection
        start_time = "2021-07-01T00:00:00Z"
        end_time = "2021-10-10T00:00:00Z"

        # apply function to retrieve tweets by user ID
        df_tweets = get_user_tweets(bearer_token = api_bearer_token, start_date = start_time, end_date = end_time, user_IDs = twitterIDs_list)

        # save dataframe to export path
        df_tweets.to_csv(f"{EXPORT_PATH}/tweets.csv")

# # # # # Twitter API: run algorithm to check users during rate limit pauses above # # # # #

    if (TASK == "Check Users" or TASK == "ALL"):
        # import twitter IDs dataset
        twitter_ids = pd.read_csv(f"{PATH}/Clean Data/Twitter/twitter_ids.csv")

        # get twitter user IDs as a list
        twitterIDs_list = twitter_ids["user_id"].tolist()
        
        # get user IDs for 4 specific users in which algorithm was paused due to Twitter's rate limits being reached
        indices = [276, 555, 822, 1074] # manually checked which indices of IDs were halted during Twitter API rate limits
        check_userID = [twitterIDs_list[i] for i in indices]

        # specify start and end time of tweet collection
        start_time = "2021-07-01T00:00:00Z"
        end_time = "2021-10-10T00:00:00Z"

        # obtain tweets by these specific users
        df_tweetcheck = get_user_tweets(bearer_token = api_bearer_token, start_date = start_time, end_date = end_time, user_IDs = check_userID)

        # save dataframe
        df_tweetcheck.to_csv(f"{EXPORT_PATH}/tweet_check.csv")

# # # # # Twitter API: get full tweet text of parent tweets in cases of Re-Tweets # # # # #

    if (TASK == "Scrape Retweets" or TASK == "ALL"):
        # re-tweets are often concated at a certain length, hence we use the parent IDs of each re-tweet to get the full text of the original tweet
        # obtain IDs of parent tweets
        parent_tweets = pd.read_csv(f"{PATH}/Clean Data/Twitter/twitter_clean.csv", dtype=str, 
                                    converters={'Likes' : int, 'Replies' : int, 'Retweets' : int, 'Quotes' : int})

        # check the columns are in the correct format
        parent_tweets.info()

        # create list of parent tweet IDs coming from retweets
        ptweet_IDs = list(parent_tweets[parent_tweets.loc[:, "Type"]=="retweeted"]["Parent Tweet ID"].unique())
        list_IDs = [ptweet_IDs[i] for i in range(0,len(ptweet_IDs))]

        # obtain full tweet text of re-tweets using IDs
        df_retweets = get_tweets_IDs(bearer_token=api_bearer_token, tweet_IDs=list_IDs)

        # save dataframe
        df_retweets.to_csv(f"{EXPORT_PATH}/retweets.csv")

# # # # # Step 4: run algorithm to get replies of all tweets # # # # #

    if (TASK == "Scrape Replies" or TASK == "ALL"):
        # get list of Tweet IDs (conversation IDs) for which there exist replies
        df_tweets = pd.read_csv(f"{PATH}/Clean Data/Twitter/twitter_clean.csv", dtype=str,
                                converters={'Likes' : int, 'Replies' : int, 'Retweets' : int, 'Quotes' : int})

        # check columns are in correct format
        df_tweets.info()

        # get dataframe of those user ids which have replies > 0
        df_tweets = df_tweets[df_tweets.loc[:, "Replies"]>0]

        # define start and end date for reply search
        start_time = "2021-07-01T00:00:00Z"
        end_time = "2021-10-15T00:00:00Z" # setting the end_time a little later than for politicians tweets, due to delayed timing of replies
            
        # obtain full text of replies
        df_replies = get_tweet_replies(bearer_token=api_bearer_token, start_date=start_time, end_date=end_time, tweet_dataframe=df_tweets)

        # save dataframe
        df_replies.to_csv(f"{EXPORT_PATH}/replies.csv", index=False)

