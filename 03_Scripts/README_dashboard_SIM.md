# Dashboard SIM — Guida operativa

## 1. Scopo

Questa cartella contiene la dashboard integrata del **Sistema Integrato di Monitoraggio (SIM)**.

La dashboard è composta da:

- una **shell principale** con homepage e navigazione comune;
- una dashboard figlia per **Conto annuale**;
- una dashboard figlia per **PA Digitale 2026**;
- una sezione predisposta per **ANAC** e per eventuali altre fonti.

La dashboard non viene avviata direttamente dai file `.Rmd`. Il punto di ingresso corretto è il runner:

```r
source("03_Scripts/06_render_dashboard_SIM_integrata.R")
```

Il runner si occupa di:

1. caricare la configurazione comune;
2. autenticarsi su Google Drive;
3. individuare e scaricare gli input;
4. salvare gli input in una cache locale temporanea;
5. avviare le dashboard figlie su porte dedicate;
6. avviare la shell principale;
7. aprire la dashboard nel browser.

## 2. Architettura

```text
Google Drive
    |
    | download input
    v
07_Temp/SIM/Dashboard/<RUN_ID>/input/
    |
    +--> Conto annuale      http://127.0.0.1:8011
    |
    +--> PA Digitale 2026   http://127.0.0.1:8012
    |
    v
Shell SIM                  http://127.0.0.1:8010
```

La shell incorpora le dashboard figlie tramite `iframe`.

| Applicazione | Porta |
|---|---:|
| Shell SIM | 8010 |
| Conto annuale | 8011 |
| PA Digitale 2026 | 8012 |

Le porte devono essere libere prima dell’avvio.

## 3. File principali

### Configurazione comune

```text
03_Scripts/00_config.R
```

Contiene i path Drive condivisi, le cartelle specifiche delle fonti, il riferimento alla master list SIM e la directory temporanea locale.

I singoli script non devono ricostruire autonomamente i path già definiti in `00_config.R`.

### Runner integrato

```text
03_Scripts/06_render_dashboard_SIM_integrata.R
```

È il punto di ingresso dell’intero sistema. Carica configurazione e helper, esegue l’autenticazione, scarica gli input, prepara i parametri e avvia shell e dashboard figlie.

### Shell principale

```text
03_Scripts/SIM/06_dashboard_SIM_integrata.Rmd
```

La shell contiene homepage, panoramica del perimetro PA, navigazione verso le fonti e contenitori `iframe`.

Riceve almeno:

```yaml
params:
  file_master_pa: null
  url_conto_annuale: "http://127.0.0.1:8011"
  url_padigitale: "http://127.0.0.1:8012"
  url_anac: null
```

Qualunque ulteriore parametro passato dal runner deve essere dichiarato anche nel blocco YAML della shell.

### Dashboard Conto annuale

```text
03_Scripts/Conto_annuale/05_dashboard_SIM_ContoAnnuale.Rmd
```

Riceve:

```yaml
params:
  file_master_ca: null
```

Il dataset viene letto localmente:

```r
master_ca <- readRDS(params$file_master_ca)
raw <- master_ca
```

La dashboard non deve autenticarsi su Drive, cercare file remoti, scaricare dati o installare pacchetti.

### Dashboard PA Digitale 2026

```text
03_Scripts/PAdigitale2026/05_dashboard_SIM_PADigitale2026.Rmd
```

Riceve:

```yaml
params:
  file_fact_dashboard: null
  file_dim_enti: null
  file_dim_avvisi: null
  file_metadata_indicatori: null
  file_metadata_filtri: null
  run_id_indicatori: null
  anno_nuts: 2024
  risoluzione_nuts: "10"
```

Anche questa dashboard legge esclusivamente file locali già scaricati dal runner.

## 4. Prerequisiti

Prima dell’avvio verificare:

1. R e RStudio installati;
2. repository aggiornato;
3. accesso all’account Google Drive configurato;
4. pacchetti R necessari già installati;
5. porte 8010, 8011 e 8012 libere;
6. working directory impostata sulla radice del progetto.

```r
getwd()
```

Il risultato deve corrispondere alla cartella principale del repository.

## 5. Aggiornamento del repository

```bash
git pull
git branch --show-current
git status --short
```

File locali, copie di backup e cartelle temporanee non devono sostituire i file canonici richiamati dal runner.

## 6. Avvio passo passo

1. Aprire il progetto RStudio dalla radice del repository.
2. Verificare che `03_Scripts/00_config.R` contenga i path richiesti.
3. Eseguire:

```r
source("03_Scripts/06_render_dashboard_SIM_integrata.R")
```

4. Completare l’autenticazione Google Drive, quando richiesta.
5. Attendere il download degli input.
6. Attendere l’avvio delle dashboard figlie.
7. Aprire:

```text
http://127.0.0.1:8010
```

Non usare direttamente **Run Document** sui file `.Rmd`, salvo test diagnostici con parametri valorizzati manualmente.

## 7. Cache locale

Gli input scaricati vengono salvati in:

```text
07_Temp/SIM/Dashboard/<RUN_ID>/input/
```

I log vengono salvati in:

```text
07_Temp/SIM/Dashboard/<RUN_ID>/logs/
```

Il valore `<RUN_ID>` cambia a ogni esecuzione.

## 8. Perché i file Rmd non sono autonomi

I file `.Rmd` ricevono i percorsi degli input tramite `params`.

Esempi:

```r
params$file_master_ca
params$file_fact_dashboard
```

Questi file vengono creati localmente dal runner dopo il download da Drive. Se un `.Rmd` viene eseguito direttamente, i parametri possono essere `NULL` e gli input locali possono non esistere.

La separazione è intenzionale:

- il runner gestisce accesso ai dati e orchestrazione;
- i file `.Rmd` gestiscono analisi e visualizzazione.

## 9. Test diretto delle applicazioni

Con il runner attivo:

```text
http://127.0.0.1:8011
```

apre Conto annuale, mentre:

```text
http://127.0.0.1:8012
```

apre PA Digitale 2026.

Interpretazione:

- pagina funzionante: la dashboard figlia è attiva;
- connessione rifiutata: il processo figlio non è partito o si è chiuso;
- errore R: leggere il relativo file `stderr`;
- caricamento bloccato: verificare input e chunk iniziali.

## 10. Log e diagnostica

File utili:

```text
06_render_dashboard_SIM_integrata.<RUN_ID>.log
conto_annuale_stdout.log
conto_annuale_stderr.log
padigitale_stdout.log
padigitale_stderr.log
```

In caso di errore:

1. controllare il log principale;
2. controllare lo `stderr` della dashboard figlia;
3. aprire direttamente la porta della dashboard figlia;
4. verificare i parametri YAML;
5. verificare i path locali degli input.

## 11. Errori frequenti

### `render params not declared in YAML`

Il runner passa un parametro non dichiarato nello YAML del file `.Rmd`.

Soluzione:

- dichiarare il parametro nello YAML;
- oppure rimuoverlo dal runner, se non utilizzato.

Runner e Rmd devono condividere lo stesso contratto di parametri.

### File input non trovato

Controllare download da Drive, path locale, nome del file, `RUN_ID` e presenza del file nella cache.

### Porta già occupata

Chiudere le sessioni precedenti o terminare i processi R rimasti attivi, poi rilanciare.

### Dashboard visibile ma non interattiva

La dashboard dipende da processi Shiny attivi. Un HTML temporaneo non è sufficiente per distribuirla come applicazione autonoma.

## 12. HTML e pubblicazione

`rmarkdown::run()` genera un HTML temporaneo in una cartella di sistema.

Questo HTML:

- non viene salvato in una posizione stabile;
- non viene caricato automaticamente su Drive;
- non è autonomo;
- dipende dal processo Shiny;
- dipende dalle dashboard figlie sulle porte locali.

Per condividere la dashboard interattiva serve una pubblicazione su Posit Connect, shinyapps.io, Shiny Server o un server interno.

## 13. Integrazione di una nuova fonte

Ogni nuova fonte deve seguire lo stesso schema.

### Nel runner

1. individuare gli input;
2. scaricarli da Drive;
3. salvarli localmente;
4. costruire una lista `params`;
5. avviare il relativo `.Rmd` su una porta dedicata.

### Nel file Rmd

1. dichiarare tutti i parametri nello YAML;
2. verificare che i file esistano;
3. leggere i file locali;
4. contenere solo logica analitica e di visualizzazione;
5. evitare autenticazione e download.

### Esempio ANAC

1. definire i path stabili in `00_config.R`;
2. produrre fact table, dimensione enti, dimensioni specifiche e metadati;
3. creare `03_Scripts/ANAC/05_dashboard_SIM_ANAC.Rmd`;
4. dichiarare i parametri nello YAML;
5. aggiungere porta, download e processo figlio al runner;
6. aggiungere l’URL e la sezione nella shell.

## 14. Regole di sviluppo condiviso

- lavorare su branch separati;
- fare commit piccoli e tematici;
- non usare `git add .`;
- non versionare dataset, log e output generati;
- non modificare il runner per aggiungere logiche analitiche;
- non duplicare i path definiti in `00_config.R`;
- non usare `install.packages()` negli script operativi;
- non usare `rm(list = ls())` dentro i file Rmd;
- mantenere i nomi canonici dei file;
- evitare suffissi `_copy`, `_old`, `_fullscreen` nei file versionati;
- testare la dashboard figlia prima della shell integrata.

## 15. Sequenza consigliata di test

1. eseguire la pipeline della fonte;
2. verificare gli output su Drive;
3. avviare il runner integrato;
4. aprire direttamente la porta della dashboard figlia;
5. verificare la shell su porta 8010;
6. controllare filtri, grafici e download;
7. controllare i log;
8. fare commit solo dei file necessari.

## 16. File da non versionare

```text
07_Temp/
05_Logs/
.RDataTmp*
*_old.Rmd
*_copy.Rmd
*_fullscreen.Rmd
old/
```

Dataset e output operativi devono essere gestiti tramite Google Drive.

## 17. Avvio rapido

```r
source("03_Scripts/06_render_dashboard_SIM_integrata.R")
```

Poi aprire:

```text
http://127.0.0.1:8010
```

## 18. Checklist

Prima dell’avvio:

- [ ] repository aggiornato;
- [ ] branch corretto;
- [ ] working directory corretta;
- [ ] accesso Drive disponibile;
- [ ] pacchetti installati;
- [ ] porte libere;
- [ ] YAML e parametri sincronizzati.

Dopo l’avvio:

- [ ] Conto annuale disponibile su 8011;
- [ ] PA Digitale disponibile su 8012;
- [ ] shell disponibile su 8010;
- [ ] log senza errori;
- [ ] filtri e grafici funzionanti.
