#!/bin/bash

cd "$(dirname "$0")"

clear

echo ""
echo "============================================================"
echo " Sistema Informativo di Monitoraggio (SIM)"
echo " Piattaforma di consultazione"
echo "============================================================"
echo ""

if ! command -v Rscript >/dev/null 2>&1; then
    echo "ERRORE"
    echo ""
    echo "R non risulta installato su questo computer."
    echo ""
    echo "Per utilizzare il SIM è necessario installare R:"
    echo "https://cran.r-project.org/"
    echo ""
    read -p "Premi INVIO per chiudere..."
    exit 1
fi

echo "✓ R trovato"
echo "✓ Cartella progetto trovata"
echo ""
echo "Avvio in corso..."
echo ""
echo "Questa fase può richiedere alcuni minuti, soprattutto al primo avvio."
echo "Il sistema sta controllando i pacchetti, accedendo a Google Drive,"
echo "scaricando i dati e avviando le dashboard."
echo ""
echo "Non chiudere questa finestra."
echo "Quando tutto sarà pronto, si aprirà automaticamente il browser."
echo ""
echo "------------------------------------------------------------"
echo ""

Rscript run_SIM_dashboard.R

echo ""
echo "------------------------------------------------------------"
echo "SIM terminato."
read -p "Premi INVIO per chiudere..."