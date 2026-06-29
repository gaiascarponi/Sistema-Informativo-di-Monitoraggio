#!/bin/bash

cd "$(dirname "$0")"

clear

echo ""
echo "============================================================"
echo " Sistema Informativo di Monitoraggio (SIM)"
echo " Piattaforma di consultazione"
echo "============================================================"
echo ""

# ============================================================
# 1. Verifica cartella progetto
# ============================================================

if [ ! -f "run_SIM_dashboard.R" ]; then
    echo "ERRORE"
    echo ""
    echo "File run_SIM_dashboard.R non trovato."
    echo "Assicurarsi di avviare il file dalla cartella principale del SIM."
    echo ""
    read -p "Premi INVIO per chiudere..."
    exit 1
fi

echo "✓ Cartella progetto trovata"

# ============================================================
# 2. Verifica R
# ============================================================

if ! command -v Rscript >/dev/null 2>&1; then
    echo ""
    echo "------------------------------------------------------------"
    echo "ERRORE: R non e' installato su questo computer."
    echo "------------------------------------------------------------"
    echo ""
    echo "Per installare R:"
    echo "  1. Aprire il browser e andare su: https://cran.r-project.org/"
    echo "  2. Cliccare su 'Download R for macOS'"
    echo "  3. Scaricare e installare il file .pkg"
    echo "  4. Riavviare il computer"
    echo "  5. Riaprire questo file"
    echo ""
    echo "Dopo l'installazione di R, installare anche Quarto:"
    echo "  https://quarto.org/docs/download/"
    echo ""
    read -p "Premi INVIO per chiudere..."
    exit 1
fi

echo "✓ R trovato"

# ============================================================
# 2b. Verifica e installazione Quarto / Pandoc
# ============================================================

echo ""
echo "Controllo Quarto / Pandoc..."

PANDOC_PATH=""

# Cerca Pandoc nelle posizioni standard di Quarto e RStudio (Mac)
PANDOC_CANDIDATES=(
    "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools"
    "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/x86_64"
    "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/arm64"
    "/Applications/RStudio.app/Contents/MacOS/pandoc"
    "/Applications/RStudio.app/Contents/Resources/pandoc"
    "$HOME/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools"
    "$HOME/Applications/RStudio.app/Contents/MacOS/pandoc"
    "/usr/local/bin"
    "/opt/homebrew/bin"
)

# Cerca anche Quarto standalone
QUARTO_BIN=""
for Q in "/usr/local/bin/quarto" "/opt/homebrew/bin/quarto" "$HOME/.local/bin/quarto" "/Applications/quarto/bin/quarto"; do
    if [ -x "$Q" ]; then
        QUARTO_BIN="$Q"
        break
    fi
done

if [ -n "$QUARTO_BIN" ]; then
    # Ricava la cartella degli strumenti di Quarto
    Q_TOOLS=$("$QUARTO_BIN" --paths 2>/dev/null | grep -i "tools" | awk '{print $2}' | head -1)
    if [ -n "$Q_TOOLS" ] && [ -f "$Q_TOOLS/pandoc" ]; then
        PANDOC_PATH="$Q_TOOLS"
    fi
    if [ -z "$PANDOC_PATH" ]; then
        Q_BIN=$(dirname "$QUARTO_BIN")
        if [ -f "$Q_BIN/tools/pandoc" ]; then
            PANDOC_PATH="$Q_BIN/tools"
        fi
    fi
fi

# Cerca nelle posizioni standard
if [ -z "$PANDOC_PATH" ]; then
    for candidate in "${PANDOC_CANDIDATES[@]}"; do
        if [ -f "$candidate/pandoc" ]; then
            PANDOC_PATH="$candidate"
            break
        fi
    done
fi

# Cerca pandoc nel PATH
if [ -z "$PANDOC_PATH" ]; then
    if command -v pandoc >/dev/null 2>&1; then
        PANDOC_PATH="$(dirname "$(command -v pandoc)")"
    fi
fi

if [ -z "$PANDOC_PATH" ]; then
    echo ""
    echo "------------------------------------------------------------"
    echo "ERRORE: Quarto non e' installato su questo computer."
    echo "------------------------------------------------------------"
    echo ""
    echo "Quarto e' necessario per avviare il SIM."
    echo ""
    echo "Per installarlo:"
    echo "  1. Aprire il browser e andare su: https://quarto.org/docs/download/"
    echo "  2. Scaricare il file .pkg per macOS"
    echo "  3. Aprire il file scaricato e seguire l'installazione"
    echo "  4. Al termine, riaprire questo file"
    echo ""
    read -p "Premi INVIO per chiudere..."
    exit 1
fi

export RSTUDIO_PANDOC="$PANDOC_PATH"
echo "✓ Quarto / Pandoc trovato"

# ============================================================
# 3. Crea cartelle locali se mancanti
# ============================================================

mkdir -p "07_Temp" "05_Logs"

# ============================================================
# 4. Avvio SIM
# ============================================================

echo ""
echo "Avvio in corso..."
echo ""
echo "Questa fase puo' richiedere alcuni minuti al primo avvio."
echo "Il sistema sta controllando i pacchetti, accedendo a Google Drive,"
echo "scaricando i dati e avviando le dashboard."
echo ""
echo "Quando richiesto, completare il login Google nel browser."
echo ""
echo "Non chiudere questa finestra."
echo "Quando tutto sara' pronto, si aprira' automaticamente il browser."
echo ""
echo "------------------------------------------------------------"
echo ""

Rscript run_SIM_dashboard.R

echo ""
echo "------------------------------------------------------------"
echo "SIM terminato."
read -p "Premi INVIO per chiudere..."
