import os

# Scraping Config
ID_START = 0
ID_END = 700

# Path Config
BASE_DIR = os.path.dirname(os.path.abspath(__file__))
OUTPUT_DIR = os.path.join(BASE_DIR, "output")
DATA_DIR = os.path.join(OUTPUT_DIR, "data")
LOG_DIR = os.path.join(OUTPUT_DIR, "logs")

#Folders Creation
os.makedirs(DATA_DIR, exist_ok=True) #output data folder
os.makedirs(LOG_DIR, exist_ok=True)  #log folder

#Output
CSV_OUTPUT = os.path.join(DATA_DIR, "webscraping_final.csv") #CSV for incremental updates
LOG_FILE = os.path.join(LOG_DIR, "scraping_log.log") #Log file
