# ============================================================
# SIM - Launcher R
# ============================================================

cat("\n")
cat("============================================================\n")
cat(" Sistema Informativo di Monitoraggio (SIM)\n")
cat(" Piattaforma di consultazione\n")
cat("============================================================\n\n")

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

cat("3/5 Avvio accesso a Google Drive...\n")
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