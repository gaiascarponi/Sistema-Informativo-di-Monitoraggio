# ============================================================
# SIM - Launcher R
# ============================================================

cat("\n")
cat("============================================================\n")
cat(" Sistema Informativo di Monitoraggio (SIM)\n")
cat(" Piattaforma di consultazione\n")
cat("============================================================\n\n")

# Crea automaticamente la cartella temporanea se non esiste
if (!dir.exists("07_Temp")) {
  dir.create("07_Temp", recursive = TRUE)
  cat("✓ Creata cartella temporanea 07_Temp\n\n")
} else {
  cat("✓ Cartella temporanea 07_Temp trovata\n\n")
}

flush.console()

args <- commandArgs(trailingOnly = FALSE)
file_arg <- "--file="
script_path <- sub(file_arg, "", args[grep(file_arg, args)])

if (length(script_path) > 0 && nzchar(script_path)) {
  project_root <- dirname(normalizePath(script_path, winslash = "/", mustWork = TRUE))
} else {
  project_root <- getwd()
}

setwd(project_root)

cat("1/5 Controllo cartella progetto...\n")
cat("Cartella progetto:\n", project_root, "\n\n", sep = "")
flush.console()

main_script <- "03_Scripts/06_render_dashboard_SIM_integrata.R"

if (!file.exists(main_script)) {
  stop(
    "Non trovo:\n",
    main_script,
    "\nControlla che run_SIM_dashboard.R sia nella root del progetto.",
    call. = FALSE
  )
}

cat("✓ Script principale trovato\n\n")
flush.console()

cat("2/5 Controllo pacchetti R...\n")
flush.console()

required_packages <- c(
  "callr",
  "googledrive",
  "rmarkdown",
  "shiny",
  "dplyr",
  "stringr",
  "readr",
  "jsonlite",
  "DT",
  "plotly",
  "ggplot2",
  "sf",
  "leaflet",
  "htmltools",
  "bslib",
  "tidyr",
  "purrr"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  cat("Pacchetti mancanti:\n")
  cat(paste0(" - ", missing_packages, collapse = "\n"))
  cat("\n\nInstallazione pacchetti mancanti...\n")
  cat("Questa operazione può richiedere alcuni minuti.\n\n")
  flush.console()
  
  install.packages(
    missing_packages,
    repos = "https://cloud.r-project.org/"
  )
} else {
  cat("✓ Tutti i pacchetti risultano già installati\n\n")
  flush.console()
}

# ============================================================
# Controllo Pandoc
# ============================================================

cat("2b/5 Controllo Pandoc...\n")
flush.console()

# Prova a trovare automaticamente Pandoc installato con RStudio/Quarto
candidate_pandoc <- c(
  Sys.getenv("RSTUDIO_PANDOC"),
  "C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools",
  "C:/Program Files/RStudio/resources/app/quarto/bin/tools",
  "C:/Program Files/Positron/resources/app/quarto/bin/tools",
  "C:/Program Files/Quarto/bin/tools"
)

candidate_pandoc <- candidate_pandoc[nzchar(candidate_pandoc)]
candidate_pandoc <- candidate_pandoc[file.exists(file.path(candidate_pandoc, "pandoc.exe"))]

if (length(candidate_pandoc) > 0) {
  Sys.setenv(RSTUDIO_PANDOC = candidate_pandoc[[1]])
}

if (!rmarkdown::pandoc_available()) {
  stop(
    paste(
      "Pandoc non trovato.",
      "",
      "Installare RStudio Desktop oppure Quarto.",
      "Se RStudio è già installato, verificare il percorso di installazione."
    ),
    call. = FALSE
  )
}

cat("✓ Pandoc trovato (", as.character(rmarkdown::pandoc_version()), ")\n\n", sep = "")
flush.console()

cat("3/5 Avvio accesso a Google Drive...\n")

# ============================================================
# Controllo Pandoc
# ============================================================

cat("2b/5 Controllo Pandoc...\n")
flush.console()

pandoc_exe <- if (.Platform$OS.type == "windows") "pandoc.exe" else "pandoc"

candidate_pandoc <- c(
  Sys.getenv("RSTUDIO_PANDOC"),
  
  # macOS - RStudio
  "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/aarch64",
  "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/x86_64",
  "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools",
  
  # macOS - Quarto / Homebrew
  "/Applications/Quarto.app/Contents/Resources/bin/tools",
  "/opt/homebrew/bin",
  "/usr/local/bin",
  
  # Windows - RStudio / Quarto
  "C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools",
  "C:/Program Files/RStudio/resources/app/quarto/bin/tools",
  "C:/Program Files/RStudio/resources/app/quarto/bin/tools/x86_64",
  "C:/Program Files/RStudio/resources/app/quarto/bin/tools/aarch64",
  "C:/Program Files/Quarto/bin/tools",
  "C:/Program Files/Quarto/bin"
)

candidate_pandoc <- candidate_pandoc[nzchar(candidate_pandoc)]
candidate_pandoc <- candidate_pandoc[file.exists(file.path(candidate_pandoc, pandoc_exe))]

if (length(candidate_pandoc) > 0) {
  Sys.setenv(RSTUDIO_PANDOC = candidate_pandoc[[1]])
}

if (!rmarkdown::pandoc_available()) {
  stop(
    paste(
      "Pandoc non trovato.",
      "",
      "Per avviare il SIM e' necessario che Pandoc sia disponibile.",
      "Pandoc e' incluso in RStudio Desktop oppure in Quarto.",
      "",
      "Soluzione consigliata:",
      "- installare o aggiornare RStudio Desktop;",
      "- in alternativa installare Quarto;",
      "- poi riavviare il launcher SIM.",
      sep = "\n"
    ),
    call. = FALSE
  )
}

cat("✓ Pandoc trovato: ", as.character(rmarkdown::pandoc_version()), "\n", sep = "")
cat("✓ Percorso Pandoc: ", Sys.getenv("RSTUDIO_PANDOC"), "\n\n", sep = "")
flush.console()

cat("Se richiesto, completare il login nel browser.\n\n")
flush.console()


cat("4/5 Download dati e preparazione input...\n")
cat("Questa fase può richiedere tempo. Attendere senza chiudere la finestra.\n\n")
flush.console()

cat("5/5 Avvio piattaforma SIM...\n")
cat("Il browser si aprirà automaticamente quando la Home sarà pronta.\n\n")
flush.console()

source(
  main_script,
  local = new.env(parent = globalenv())
)