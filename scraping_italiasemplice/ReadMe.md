# Web Scraping - Italia Semplice

[![Python Version](https://img.shields.io/badge/python-3.12%2B-blue.svg)](https://www.python.org/)
[![Selenium](https://img.shields.io/badge/library-Selenium-green.svg)](https://www.selenium.dev/)
[![Pandas](https://img.shields.io/badge/library-Pandas-orange.svg)](https://pandas.pydata.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

**Automazione avanzata per l'estrazione sistematica degli interventi di semplificazione amministrativa dal portale nazionale.**


## Descrizione
Questa pipeline è stata sviluppata per l'estrazione di dati dal portale istituzionale [Italia Semplice](https://www.italiasemplice.gov.it). 

Viene gestita la navigazione dinamica tra le procedure, gestito il caricamento asincrono dei contenuti ed estratti i dettagli degli interventi organizzandoli in un formato strutturato (CSV/Excel) pronto per l'analisi statistica o l'integrazione in database.

---

## Architettura e Struttura
```text
scraping_italiasemplice/
├── main.py                # Coordina il flusso e il salvataggio dati
├── config.py              # Configurazioni generali 
├── scraper_engine.py      # Gestione del driver Selenium (Logica Anti-bot)
├── parser_utils.py        # Logica di estrazione (Regex e selettori XPATH)
├── requirements.txt       # Elenco delle dipendenze Python
└── output/                # Cartella Output
    ├── data/              # Risultati finali  (webscraping_final.csv)
    └── logs/              # Log di esecuzione (scraping_log.log)
```

# Requisiti e Installazione

- Python 3.12+

- Google Chrome installato

- Installazione dipendenze:
Apri il terminale nella cartella del progetto e digita:

``` code
pip install -r requirements.txt
```


# Utilizzo
## 1. Configurazione

È possibile definire l'intervallo di ID da analizzare modificando il file config.py:

``` python
ID_START = 0
ID_END = 700
``` 

## 2. Esecuzione

Avvia lo script principale:

``` code
python3 main.py
``` 

## 3. Monitoraggio

Il sistema fornisce feedback in tempo reale tramite:
- Terminale: Avanzamento e ID correntemente elaborati.
- Log file: Dettagli tecnici e segnalazione di eventuali pagine vuote in output/logs/scraping_log.log.


# Scelte Progettuali e Problem Solving

Per rendere la pipeline professionale e resiliente, sono state implementate le seguenti soluzioni:

- **Stabilità**: Il salvataggio è incrementale. Se lo script si ferma, i dati scaricati fino a quel momento sono già salvati nel CSV.

- **Gestione Anti-Bot**: Utilizzo di undetected-chromedriver per simulare un comportamento umano e minimizzare i rischi di blocco da parte del server.

- **Memory Management**: Il browser viene riavviato automaticamente ogni 20 procedure per liberare la RAM.

- **Parsing Adattivo**: Utilizzo combinato di XPATH e Regex per gestire variazioni nel layout HTML tra le diverse schede.

# Dati Estratti

Il file finale webscraping_final.csv contiene:

- Dati Procedura: ID, Nome Procedura, Settore, Categoria, Beneficiario, Tipo PA Responsabile.

- Dati Intervento: Intervento, Anno, Descrizione Intervento, Tipo Intervento, Natura Intervento e Riferimenti.

- Metadata: URL diretto per la verifica manuale del dato.

Sviluppato da: MIPA 
Data: Marzo 2026
