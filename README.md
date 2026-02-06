# Monitoraggio-PNRR

Analisi esplorativa e dashboard per il dataset **ItaliaSemplice – Dettaglio Procedure**  
(catalogo delle procedure amministrative e regolatorie).

Questo repository contiene **solo codice e documentazione**.  
Il dataset viene scaricato manualmente da Microsoft Teams / SharePoint e salvato **localmente** in una cartella `data/` che **non è tracciata da Git**.

---

## 1. Prerequisiti

Per lavorare con questo repository sono necessari:

- Git
- R (versione ≥ 4.2 consigliata)
- RStudio (consigliato)

### Nota per macOS
Su macOS è consigliabile evitare di clonare il repository all’interno di `Documents`, `Desktop` o altre cartelle protette, poiché potrebbero verificarsi problemi di permessi con il Terminal.

**Percorso consigliato:**
- macOS / Linux: `~/Projects`
- Windows: `C:\Projects`

---

## 2. Clonare il repository in locale

### Opzione A — SSH (consigliata se usi già SSH con GitHub)

```bash
cd ~/Projects
git clone git@github.com:gaiascarponi/Monitoraggio-PNRR.git
cd Monitoraggio-PNRR
```

### Opzione B — HTTPS

```bash
cd ~/Projects
git clone https://github.com/gaiascarponi/Monitoraggio-PNRR.git
cd Monitoraggio-PNRR
```


---

## 3. Aprire il progetto R

Questo repository include già il file di progetto RStudio:

```bash
Monitoraggio-PNRR.Rproj
```

È possibile aprirlo:

* facendo doppio clic su Monitoraggio-PNRR.Rproj, oppure

* in RStudio: **File → Open Project…** e selezionando il file.

**⚠️ Non creare un nuovo progetto R in un’altra cartella.**
La root del progetto deve coincidere con la root del repository, affinché i percorsi relativi funzionino correttamente.

---

## 4. Struttura delle cartelle

Struttura attesa (semplificata):

```bash
Monitoraggio-PNRR/
  Monitoraggio-PNRR.Rproj
  README.md
  .gitignore
  data/                  # dati locali (non tracciati da Git)
  import_data.R          # script (tracciati)
```

**Informazioni sulla cartella data/**

* La cartella data/ deve esistere in locale.

* Tutti i file di dati reali devono essere salvati all’interno di data/.

* I file presenti in data/ sono ignorati da Git per progettazione e non devono mai essere committati.

---

## 5. Scaricare i dati e salvarli nella cartella corretta

1. Scaricare il file CSV da Microsoft Teams / SharePoint utilizzando il link riportato sotto.

2. Salvare il file nella cartella:
```bash
  Monitoraggio-PNRR/data/
```

3. Utilizzare il seguente nome file (consigliato e atteso dagli script):
```bash
  ItaliaSemplice_DettaglioProcedure.csv
```

**Fonte dati (Teams / SharePoint)**
```bash
  https://mipaconsorzio.sharepoint.com/:x:/r/sites/Istat-MonitoraggioriformePA/Shared%20Documents/Istat%20-%20Monitoraggio%20riforma%20PA/ItaliaSemplice_DettaglioProcedure.csv?d=wdb595566661646d28689b2dc1898a942&csf=1&web=1&e=yuClnc
```

---

## 6. Eseguire l’importazione / analisi

Dopo aver posizionato il file dei dati nella cartella data/, aprire il progetto R ed eseguire lo script di importazione, ad esempio:
```bash
  source("import_data.R")
```
Se uno script restituisce un errore perché il file di dati non viene trovato, verificare che:

* il file esista effettivamente nella cartella data/

* il nome del file corrisponda esattamente (attenzione a maiuscole/minuscole)

* il progetto R (.Rproj) sia aperto, in modo che la working directory sia corretta

---

## 7. Regole di collaborazione (importante)

* Non eseguire commit di file di dati (CSV, XLSX, ecc.).

* Non eseguire commit di credenziali o segreti (ad es. .Renviron).

* Eseguire commit solo di codice, documentazione e template di configurazione.

Workflow suggerito:

1. Aggiornare il repository locale: **git pull**

2. Apportare le modifiche in locale

3. Committare solo codice o documentazione rilevante

4. Effettuare il push sul repository (oppure aprire una Pull Request, se il team utilizza le PR)

