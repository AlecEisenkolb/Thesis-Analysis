# Twitter API 
# Author: Alec Eisenkolb
# Date: 01.05.2022

# load libraries
import pandas as pd
import tweepy
import yaml
import os

# define function to load YAML files
def yaml_import(file_name):
    """
    This function loads a YAML file. Inputs: 
    - file_name: name of file, encoded as string.
    """
    # use path to load yaml file
    with open(file_name, "r") as path:
        config = yaml.load(path, Loader = yaml.FullLoader)
    # return data
    return config

# Define file_name of config file and import data
file_name = f"{os.path.dirname(os.path.realpath(__file__))}/config.yml"
config = yaml_import(file_name)

# import twitter IDs dataset
twitter_ids = pd.read_csv(r'/Users/alec/Dropbox/HSG MEcon/Master Thesis/Thesis Analysis/Clean Data/Twitter/twitter_ids.csv')
