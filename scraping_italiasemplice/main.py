import logging
import pandas as pd
import time
import os
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
import config
from scraper_engine import get_driver
from parser_utils import extract_label, extract_procedure_name, parse_interventions

# Setup Logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s [%(levelname)s] %(message)s',
    handlers=[
        logging.FileHandler(config.LOG_FILE),
        logging.StreamHandler()
    ]
)

def run_pipeline():
    logging.info("Avvio scraping")
    driver = None
    
    for current_id in range(config.ID_START, config.ID_END + 1):
        url = f"https://www.italiasemplice.gov.it/ricerca/dettaglio/{current_id}"
        logging.info(f"Elaborazione ID {current_id}: {url}")

        try:
            if driver is None: driver = get_driver()
            driver.get(url)
            try:
                WebDriverWait(driver, 5).until(EC.presence_of_element_located((By.XPATH, "//*[contains(text(), 'Settore:')]")))
            except:
                logging.warning(f"ID {current_id}: Pagina vuota o non trovata")
                continue

            #data extraction
            body_text = driver.find_element(By.TAG_NAME, "body").text
            nome_proc = extract_procedure_name(driver)
            settore = extract_label(body_text, "Settore")
            categoria = extract_label(body_text, "Categoria")
            beneficiario = extract_label(body_text, "Beneficiario")
            tipo_pa = extract_label(body_text, "Tipo PA Responsabile")
            interventions = parse_interventions(driver) #interventi

            rows = [] #list of dicts to store data for each intervention
            for inter in interventions:
                row = {
                    "ID": current_id,
                    "Nome Procedura": nome_proc,
                    "Settore": settore,
                    "Categoria": categoria,
                    "Beneficiario": beneficiario,
                    "Tipo PA Responsabile": tipo_pa,
                    **inter,
                    "URL": url
                }
                rows.append(row)

            #Incremental CSV writing/saving
            df_temp = pd.DataFrame(rows)
            file_exists = os.path.isfile(config.CSV_OUTPUT)
            df_temp.to_csv(config.CSV_OUTPUT, mode='a', index=False, header=not file_exists, sep=';', encoding='utf-8-sig')
            
            logging.info(f"ID {current_id} scaricato con successo ({len(rows)} interventi salvati)")

            if current_id % 20 == 0: #memory management: restart driver every 20 IDs
                driver.quit()
                driver = None

        except Exception as e:
            logging.error(f"Errore critico su ID {current_id}: {e}")
            if driver: 
                driver.quit()
                driver = None

    if driver: driver.quit()
    logging.info("Scraping completato")

if __name__ == "__main__":
    run_pipeline()
