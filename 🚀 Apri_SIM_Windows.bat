@echo off
setlocal enabledelayedexpansion
cd /d "%~dp0"

cls

echo.
echo ============================================================
echo  Sistema Informativo di Monitoraggio ^(SIM^)
echo  Piattaforma di consultazione
echo ============================================================
echo.

REM ============================================================
REM 1. Verifica cartella progetto
REM ============================================================

if not exist "run_SIM_dashboard.R" (
    echo ERRORE
    echo.
    echo File run_SIM_dashboard.R non trovato nella cartella corrente:
    echo %CD%
    echo.
    echo Assicurarsi di avviare apriSIM.bat dalla cartella principale del SIM.
    echo.
    pause
    exit /b 1
)

echo [OK] Cartella progetto trovata

REM ============================================================
REM 2. Ricerca Rscript
REM ============================================================

set "RSCRIPT="

REM Cerca nel PATH
where Rscript >nul 2>nul
if not errorlevel 1 (
    set "RSCRIPT=Rscript"
)

REM Cerca nelle installazioni standard
if "%RSCRIPT%"=="" (
    for /d %%D in ("C:\Program Files\R\R-*") do (
        if exist "%%D\bin\Rscript.exe" (
            set "RSCRIPT=%%D\bin\Rscript.exe"
        )
    )
)

REM Cerca in bin\x64
if "%RSCRIPT%"=="" (
    for /d %%D in ("C:\Program Files\R\R-*") do (
        if exist "%%D\bin\x64\Rscript.exe" (
            set "RSCRIPT=%%D\bin\x64\Rscript.exe"
        )
    )
)

REM Cerca anche in Program Files (x86), per sicurezza
if "%RSCRIPT%"=="" (
    for /d %%D in ("C:\Program Files (x86)\R\R-*") do (
        if exist "%%D\bin\Rscript.exe" (
            set "RSCRIPT=%%D\bin\Rscript.exe"
        )
    )
)

if "%RSCRIPT%"=="" (
    echo.
    echo ERRORE
    echo.
    echo R non risulta installato oppure Rscript.exe non e' stato trovato.
    echo.
    echo Per utilizzare il SIM e' necessario installare R:
    echo https://cran.r-project.org/
    echo.
    echo Dopo l'installazione, riaprire questo file.
    echo.
    pause
    exit /b 1
)

echo [OK] R trovato:
echo      %RSCRIPT%

REM ============================================================
REM 3. Prepara cartelle locali
REM ============================================================

if not exist "07_Temp" mkdir "07_Temp"
if not exist "05_Logs" mkdir "05_Logs"

echo [OK] Cartelle temporanee/log verificate

REM ============================================================
REM 4. Avvio SIM
REM ============================================================

echo.
echo Avvio in corso...
echo.
echo Questa fase puo' richiedere alcuni minuti, soprattutto al primo avvio.
echo Il sistema sta controllando i pacchetti, accedendo a Google Drive,
echo scaricando i dati e avviando la dashboard.
echo.
echo Non chiudere questa finestra.
echo Quando tutto sara' pronto, si aprira' automaticamente il browser.
echo.
echo ------------------------------------------------------------
echo.

"%RSCRIPT%" "run_SIM_dashboard.R"

set EXITCODE=%ERRORLEVEL%

echo.
echo ------------------------------------------------------------

if not "%EXITCODE%"=="0" (
    echo ERRORE
    echo.
    echo Il SIM si e' interrotto con codice errore: %EXITCODE%
    echo.
    echo Controllare eventuali messaggi sopra o i file di log.
    echo.
    pause
    exit /b %EXITCODE%
)

echo SIM terminato correttamente.
echo.

pause
exit /b 0