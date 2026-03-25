import undetected_chromedriver as uc

def get_options():
    options = uc.ChromeOptions()
    options.add_argument('--headless=new')
    options.add_argument('--no-sandbox')
    options.add_argument('--disable-gpu')
    options.add_argument('--window-size=1920,1080')
    return options

def get_driver():
    try:
        driver = uc.Chrome(options=get_options(), version_main=143) #specific version of Chrome, adjust as needed
    except Exception as e:
        driver = uc.Chrome(options=get_options())
    return driver
