@echo off
cd /d "%~dp0"

cls

echo.
echo ============================================================
echo  Sistema Informativo di Monitoraggio (SIM)
echo  Piattaforma di consultazione
echo ============================================================
echo.

where Rscript >nul 2>nul

if errorlevel 1 (
    echo ERRORE
    echo.
    echo R non risulta installato oppure Rscript non e' disponibile.
    echo.
    echo Per utilizzare il SIM e' necessario installare R:
    echo https://cran.r-project.org/
    echo.
    echo Dopo l'installazione, riaprire questo file.
    echo.
    pause
    exit /b 1
)

echo [OK] R trovato
echo [OK] Cartella progetto trovata
echo.
echo Avvio in corso...
echo.
echo Questa fase puo' richiedere alcuni minuti, soprattutto al primo avvio.
echo Il sistema sta controllando i pacchetti, accedendo a Google Drive,
echo scaricando i dati e avviando le dashboard.
echo.
echo Non chiudere questa finestra.
echo Quando tutto sara' pronto, si aprira' automaticamente il browser.
echo.
echo ------------------------------------------------------------
echo.

Rscript run_SIM_dashboard.R

echo.
echo ------------------------------------------------------------
echo SIM terminato.
echo.
pause