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
    echo File run_SIM_dashboard.R non trovato nella cartella corrente.
    echo Assicurarsi di avviare questo file dalla cartella principale del SIM.
    echo.
    pause
    exit /b 1
)

echo [OK] Cartella progetto trovata

REM ============================================================
REM 2. Ricerca Rscript
REM ============================================================

set "RSCRIPT="

where Rscript >nul 2>nul
if not errorlevel 1 (
    for /f "delims=" %%P in ('where Rscript') do (
        if "!RSCRIPT!"=="" set "RSCRIPT=%%P"
    )
)

if "%RSCRIPT%"=="" (
    for /d %%D in ("C:\Program Files\R\R-*") do (
        if exist "%%D\bin\Rscript.exe" set "RSCRIPT=%%D\bin\Rscript.exe"
    )
)

if "%RSCRIPT%"=="" (
    for /d %%D in ("C:\Program Files\R\R-*") do (
        if exist "%%D\bin\x64\Rscript.exe" set "RSCRIPT=%%D\bin\x64\Rscript.exe"
    )
)

if "%RSCRIPT%"=="" (
    for /d %%D in ("C:\Program Files (x86)\R\R-*") do (
        if exist "%%D\bin\Rscript.exe" set "RSCRIPT=%%D\bin\Rscript.exe"
    )
)

if "%RSCRIPT%"=="" (
    echo.
    echo ============================================================
    echo  ERRORE: R non e' installato su questo computer.
    echo ============================================================
    echo.
    echo Per installare R:
    echo   1. Aprire il browser e andare su: https://cran.r-project.org/
    echo   2. Cliccare su "Download R for Windows"
    echo   3. Cliccare su "base"
    echo   4. Scaricare e installare il file .exe
    echo   5. Riavviare il computer
    echo.
    echo Dopo l'installazione di R, installare anche Quarto:
    echo   https://quarto.org/docs/download/
    echo.
    echo Poi riaprire questo file.
    echo.
    pause
    exit /b 1
)

echo [OK] R trovato

REM ============================================================
REM 2b. Ricerca Quarto / Pandoc
REM ============================================================

echo.
echo Controllo Quarto / Pandoc...

set "PANDOC_PATH="

REM Quarto standalone - posizioni standard Windows
for %%D in (
    "%LOCALAPPDATA%\Programs\Quarto\bin\tools"
    "%PROGRAMFILES%\Quarto\bin\tools"
    "%LOCALAPPDATA%\Programs\Quarto\bin"
    "%PROGRAMFILES%\Quarto\bin"
) do (
    if "!PANDOC_PATH!"=="" (
        if exist "%%~D\pandoc.exe" set "PANDOC_PATH=%%~D"
    )
)

REM RStudio - posizioni standard Windows
if "%PANDOC_PATH%"=="" (
    for %%D in (
        "%LOCALAPPDATA%\Programs\RStudio\resources\app\quarto\bin\tools"
        "%PROGRAMFILES%\RStudio\resources\app\quarto\bin\tools"
        "%LOCALAPPDATA%\Programs\RStudio\bin\pandoc"
        "%PROGRAMFILES%\RStudio\bin\pandoc"
    ) do (
        if "!PANDOC_PATH!"=="" (
            if exist "%%~D\pandoc.exe" set "PANDOC_PATH=%%~D"
        )
    )
)

REM Pandoc standalone nel PATH
if "%PANDOC_PATH%"=="" (
    where pandoc >nul 2>nul
    if not errorlevel 1 (
        for /f "delims=" %%P in ('where pandoc') do (
            if "!PANDOC_PATH!"=="" set "PANDOC_PATH=%%~dpP"
        )
    )
)

if "%PANDOC_PATH%"=="" (
    echo.
    echo ============================================================
    echo  ERRORE: Quarto non e' installato su questo computer.
    echo ============================================================
    echo.
    echo Quarto e' necessario per avviare il SIM.
    echo.
    echo Per installarlo:
    echo   1. Aprire il browser e andare su: https://quarto.org/docs/download/
    echo   2. Scaricare il file .exe per Windows
    echo   3. Aprire il file scaricato e seguire l'installazione
    echo   4. Al termine, riaprire questo file
    echo.
    pause
    exit /b 1
)

set "RSTUDIO_PANDOC=%PANDOC_PATH%"
echo [OK] Quarto / Pandoc trovato

REM ============================================================
REM 3. Crea cartelle locali se mancanti
REM ============================================================

if not exist "07_Temp" mkdir "07_Temp"
if not exist "05_Logs" mkdir "05_Logs"

echo [OK] Cartelle di lavoro verificate

REM ============================================================
REM 4. Avvio SIM
REM ============================================================

echo.
echo Avvio in corso...
echo.
echo Questa fase puo' richiedere alcuni minuti al primo avvio.
echo Il sistema sta controllando i pacchetti, accedendo a Google Drive,
echo scaricando i dati e avviando le dashboard.
echo.
echo Quando richiesto, completare il login Google nel browser.
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
    echo.
    echo ERRORE: Il SIM si e' interrotto ^(codice: %EXITCODE%^).
    echo Controllare i messaggi sopra o i file in 05_Logs\
    echo.
    pause
    exit /b %EXITCODE%
)

echo SIM terminato correttamente.
echo.
pause
exit /b 0
