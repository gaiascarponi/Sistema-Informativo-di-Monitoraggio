# Tabelle descrittive Italia Semplice ----

## Import ----

#install.packages("writexl")
library(dplyr)
library(writexl)
library(purrr)
library(tidyr)

## Setup & Globals ----

if (!dir.exists("output")) {
  dir.create("output")
}
intput_path<- "data"
output_path<- "output"
df_357 <- read.csv("data/scraping_357proc.csv", sep = ",")
df_430 <- read.csv("data/scraping_430proc.csv", sep = "\t")

## Data Manipulation ----

df_357_completo <- df_430[df_430$ID %in% df_357$ID, ]
df_mancanti <- df_430 %>% anti_join(df_357, by = "ID")

## Tables ----

################################################################################
#                            df_430 
################################################################################
tab_0a <- data.frame(
  Categoria = c("Procedure", "Interventi"),
  n = c(n_distinct(df_430$ID), nrow(df_430))
) 
print(tab_0a)

tab_procedure_interventi_a <- df_430 %>%
  group_by(ID) %>%
  summarise(n_interventi = n()) %>%
  mutate(
    Categoria = case_when(
      n_interventi == 1 ~ "1 intervento",
      n_interventi == 2 ~ "2 interventi",
      n_interventi == 3 ~ "3 interventi",
      n_interventi >= 4 ~ "4+ interventi"
    )
  ) %>%
  group_by(Categoria) %>%
  summarise(n = n()) %>%
  mutate(
    `%` = round((n / sum(n)) * 100, 2)
  ) %>%
  arrange(match(Categoria, c("1 intervento", "2 interventi", "3 interventi", "4+ interventi")))
print(tab_procedure_interventi_a)

# 1. Tabella Settore
tab_1a <- df_430 %>%
  group_by(Settore) %>%
  summarise(
    Procedure_Uniche = n_distinct(ID)
  ) %>%
  mutate(
    Perc_Procedure = round((Procedure_Uniche / sum(Procedure_Uniche)) * 100, 2)
  ) %>%
  select(Settore, Procedure_Uniche, Perc_Procedure) %>%
  arrange(desc(Procedure_Uniche))
print(tab_1a)

# 2. Tabella Beneficiario
tab_2a <- df_430 %>%
  group_by(Beneficiario) %>%
  summarise(
    Procedure_Uniche = n_distinct(ID)
  ) %>%
  mutate(
    Perc_Procedure = round((Procedure_Uniche / sum(Procedure_Uniche)) * 100, 2)
  ) %>%
  select(Beneficiario, Procedure_Uniche, Perc_Procedure) %>%
  arrange(desc(Procedure_Uniche))
print(tab_2a)

# 3. Tabella per Categoria
tab_3a <- df_430 %>%
  group_by(Categoria) %>%
  summarise(
    Procedure_Uniche = n_distinct(ID)
  ) %>%
  mutate(
    Perc_Procedure = round((Procedure_Uniche / sum(Procedure_Uniche)) * 100, 2)
  ) %>%
  select(Categoria, Procedure_Uniche, Perc_Procedure) %>%
  arrange(desc(Procedure_Uniche))
print(tab_3a)

# 4. Tabella per Tipo PA Responsabile
tab_4a <- df_430 %>%
  group_by(Tipo.PA.Responsabile) %>%
  summarise(
    Procedure_Uniche = n_distinct(ID)
  ) %>%
  mutate(
    Perc_Procedure = round((Procedure_Uniche / sum(Procedure_Uniche)) * 100, 2)
  ) %>%
  select(Tipo.PA.Responsabile, Procedure_Uniche, Perc_Procedure) %>%
  arrange(desc(Procedure_Uniche))
print(tab_4a)

#INTERVENTI
#Anno
tab_5a <- df_430 %>%
  group_by(Anno) %>%
  summarise(
    Totale_Interventi = n()
  ) %>%
  mutate(
    Perc_Interventi = round((Totale_Interventi / sum(Totale_Interventi)) * 100, 2)
  ) %>%
  select(Anno, Totale_Interventi, Perc_Interventi) %>%
  arrange(desc(Totale_Interventi))
print(tab_5a)

#Tipo Intervento
tab_6a <- df_430 %>%
  group_by(Tipo.Intervento) %>%
  summarise(
    Totale_Interventi = n()
  ) %>%
  mutate(
    Perc_Interventi = round((Totale_Interventi / sum(Totale_Interventi)) * 100, 2)
  ) %>%
  select(Tipo.Intervento, Totale_Interventi, Perc_Interventi) %>%
  arrange(desc(Totale_Interventi))
print(tab_6a)

#Natura Intervento
tab_7a <- df_430 %>%
  group_by(Natura.Intervento) %>%
  summarise(
    Totale_Interventi = n()
  ) %>%
  mutate(
    Perc_Interventi = round((Totale_Interventi / sum(Totale_Interventi)) * 100, 2)
  ) %>%
  select(Natura.Intervento, Totale_Interventi, Perc_Interventi) %>%
  arrange(desc(Totale_Interventi))
print(tab_7a)

################################################################################

tabella1 <- list(
  "Settore" = tab_1a %>% rename(Categoria = 1),
  "Beneficiario" = tab_2a %>% rename(Categoria = 1),
  "Categoria" = tab_3a %>% rename(Categoria = 1),
  "Tipo_PA" = tab_4a %>% rename(Categoria = 1)
) %>% 
  bind_rows(.id = "Nome variabile") %>% 
  rename(n = Procedure_Uniche, `%` = Perc_Procedure)
tabella2 <- list(
  "Anno" = tab_5a %>% rename(Categoria = 1) %>% mutate(Categoria = as.character(Categoria)),
  "Tipo di Intervento" = tab_6a %>% rename(Categoria = 1),
  "Natura Intervento" = tab_7a %>% rename(Categoria = 1)
) %>% 
  bind_rows(.id = "Nome variabile") %>% 
  rename(n = Totale_Interventi, `%` = Perc_Interventi)

write_xlsx(
  list("Overall" = tab_0a, "Distribuzione Interventi"= tab_procedure_interventi_a ,"Procedure" = tabella1, "Interventi" = tabella2), 
  "output/Report_430.xlsx"
)

################################################################################
#                            df_357
################################################################################
tab_0b <- data.frame(
  Categoria = c("Procedure", "Interventi"),
  n = c(n_distinct(df_357_completo$ID), nrow(df_357_completo))
) 
print(tab_0b)

tab_procedure_interventi_b <- df_357_completo %>%
  group_by(ID) %>%
  summarise(n_interventi = n()) %>%
  mutate(
    Categoria = case_when(
      n_interventi == 1 ~ "1 intervento",
      n_interventi == 2 ~ "2 interventi",
      n_interventi == 3 ~ "3 interventi",
      n_interventi >= 4 ~ "4+ interventi"
    )
  ) %>%
  group_by(Categoria) %>%
  summarise(n = n()) %>%
  mutate(
    `%` = round((n / sum(n)) * 100, 2)
  ) %>%
  arrange(match(Categoria, c("1 intervento", "2 interventi", "3 interventi", "4+ interventi")))
print(tab_procedure_interventi_b)

# 1. Tabella Settore
tab_1b <- df_357_completo %>%
  group_by(Settore) %>%
  summarise(
    Procedure_Uniche = n_distinct(ID)
  ) %>%
  mutate(
    Perc_Procedure = round((Procedure_Uniche / sum(Procedure_Uniche)) * 100, 2)
  ) %>%
  select(Settore, Procedure_Uniche, Perc_Procedure) %>%
  arrange(desc(Procedure_Uniche))

# 2. Tabella Beneficiario
tab_2b <- df_357_completo %>%
  group_by(Beneficiario) %>%
  summarise(
    Procedure_Uniche = n_distinct(ID)
  ) %>%
  mutate(
    Perc_Procedure = round((Procedure_Uniche / sum(Procedure_Uniche)) * 100, 2)
  ) %>%
  select(Beneficiario, Procedure_Uniche, Perc_Procedure) %>%
  arrange(desc(Procedure_Uniche))

# 3. Tabella per Categoria
tab_3b <- df_357_completo %>%
  group_by(Categoria) %>%
  summarise(
    Procedure_Uniche = n_distinct(ID)
  ) %>%
  mutate(
    Perc_Procedure = round((Procedure_Uniche / sum(Procedure_Uniche)) * 100, 2)
  ) %>%
  select(Categoria, Procedure_Uniche, Perc_Procedure) %>%
  arrange(desc(Procedure_Uniche))

# 4. Tabella per Tipo PA Responsabile
tab_4b <- df_357_completo %>%
  group_by(Tipo.PA.Responsabile) %>%
  summarise(
    Procedure_Uniche = n_distinct(ID)
  ) %>%
  mutate(
    Perc_Procedure = round((Procedure_Uniche / sum(Procedure_Uniche)) * 100, 2)
  ) %>%
  select(Tipo.PA.Responsabile, Procedure_Uniche, Perc_Procedure) %>%
  arrange(desc(Procedure_Uniche))

#INTERVENTI
#Anno
tab_5b <- df_357_completo %>%
  group_by(Anno) %>%
  summarise(
    Totale_Interventi = n()
  ) %>%
  mutate(
    Perc_Interventi = round((Totale_Interventi / sum(Totale_Interventi)) * 100, 2)
  ) %>%
  select(Anno, Totale_Interventi, Perc_Interventi) %>%
  arrange(desc(Totale_Interventi))

#Tipo Intervento
tab_6b <- df_357_completo %>%
  group_by(Tipo.Intervento) %>%
  summarise(
    Totale_Interventi = n()
  ) %>%
  mutate(
    Perc_Interventi = round((Totale_Interventi / sum(Totale_Interventi)) * 100, 2)
  ) %>%
  select(Tipo.Intervento, Totale_Interventi, Perc_Interventi) %>%
  arrange(desc(Totale_Interventi))

#Natura Intervento
tab_7b <- df_357_completo %>%
  group_by(Natura.Intervento) %>%
  summarise(
    Totale_Interventi = n()
  ) %>%
  mutate(
    Perc_Interventi = round((Totale_Interventi / sum(Totale_Interventi)) * 100, 2)
  ) %>%
  select(Natura.Intervento, Totale_Interventi, Perc_Interventi) %>%
  arrange(desc(Totale_Interventi))

################################################################################

tabella1b <- list(
  "Settore" = tab_1b %>% rename(Categoria = 1),
  "Beneficiario" = tab_2b %>% rename(Categoria = 1),
  "Categoria" = tab_3b %>% rename(Categoria = 1),
  "Tipo_PA" = tab_4b %>% rename(Categoria = 1)
) %>% 
  bind_rows(.id = "Nome variabile") %>% 
  rename(n = Procedure_Uniche, `%` = Perc_Procedure)
tabella2b <- list(
  "Anno" = tab_5b %>% rename(Categoria = 1) %>% mutate(Categoria = as.character(Categoria)),
  "Tipo di Intervento" = tab_6b %>% rename(Categoria = 1),
  "Natura Intervento" = tab_7b %>% rename(Categoria = 1)
) %>% 
  bind_rows(.id = "Nome variabile") %>% 
  rename(n = Totale_Interventi, `%` = Perc_Interventi)

write_xlsx(
  list("Overall" = tab_0b, "Distribuzione Interventi"= tab_procedure_interventi_b ,"Procedure" = tabella1b, "Interventi" = tabella2b), 
  "output/Report_357.xlsx"
)

################################################################################
#                            df_mancanti
################################################################################
tab_0c <- data.frame(
  Categoria = c("Procedure", "Interventi"),
  n = c(n_distinct(df_mancanti$ID), nrow(df_mancanti))
) 
print(tab_0c)

tab_procedure_interventi_c <- df_mancanti %>%
  group_by(ID) %>%
  summarise(n_interventi = n()) %>%
  mutate(
    Categoria = case_when(
      n_interventi == 1 ~ "1 intervento",
      n_interventi == 2 ~ "2 interventi",
      n_interventi == 3 ~ "3 interventi",
      n_interventi >= 4 ~ "4+ interventi"
    )
  ) %>%
  group_by(Categoria) %>%
  summarise(n = n()) %>%
  mutate(
    `%` = round((n / sum(n)) * 100, 2)
  ) %>%
  arrange(match(Categoria, c("1 intervento", "2 interventi", "3 interventi", "4+ interventi")))
print(tab_procedure_interventi_c)


# 1. Tabella Settore
tab_1c <- df_mancanti %>%
  group_by(Settore) %>%
  summarise(
    Procedure_Uniche = n_distinct(ID)
  ) %>%
  mutate(
    Perc_Procedure = round((Procedure_Uniche / sum(Procedure_Uniche)) * 100, 2)
  ) %>%
  select(Settore, Procedure_Uniche, Perc_Procedure) %>%
  arrange(desc(Procedure_Uniche))

# 2. Tabella Beneficiario
tab_2c <- df_mancanti %>%
  group_by(Beneficiario) %>%
  summarise(
    Procedure_Uniche = n_distinct(ID)
  ) %>%
  mutate(
    Perc_Procedure = round((Procedure_Uniche / sum(Procedure_Uniche)) * 100, 2)
  ) %>%
  select(Beneficiario, Procedure_Uniche, Perc_Procedure) %>%
  arrange(desc(Procedure_Uniche))

# 3. Tabella per Categoria
tab_3c <- df_mancanti %>%
  group_by(Categoria) %>%
  summarise(
    Procedure_Uniche = n_distinct(ID)
  ) %>%
  mutate(
    Perc_Procedure = round((Procedure_Uniche / sum(Procedure_Uniche)) * 100, 2)
  ) %>%
  select(Categoria, Procedure_Uniche, Perc_Procedure) %>%
  arrange(desc(Procedure_Uniche))

# 4. Tabella per Tipo PA Responsabile
tab_4c <- df_mancanti %>%
  group_by(Tipo.PA.Responsabile) %>%
  summarise(
    Procedure_Uniche = n_distinct(ID)
  ) %>%
  mutate(
    Perc_Procedure = round((Procedure_Uniche / sum(Procedure_Uniche)) * 100, 2)
  ) %>%
  select(Tipo.PA.Responsabile, Procedure_Uniche, Perc_Procedure) %>%
  arrange(desc(Procedure_Uniche))

#INTERVENTI
#Anno
tab_5c <- df_mancanti %>%
  group_by(Anno) %>%
  summarise(
    Totale_Interventi = n()
  ) %>%
  mutate(
    Perc_Interventi = round((Totale_Interventi / sum(Totale_Interventi)) * 100, 2)
  ) %>%
  select(Anno, Totale_Interventi, Perc_Interventi) %>%
  arrange(desc(Totale_Interventi))

#Tipo Intervento
tab_6c <- df_mancanti %>%
  group_by(Tipo.Intervento) %>%
  summarise(
    Totale_Interventi = n()
  ) %>%
  mutate(
    Perc_Interventi = round((Totale_Interventi / sum(Totale_Interventi)) * 100, 2)
  ) %>%
  select(Tipo.Intervento, Totale_Interventi, Perc_Interventi) %>%
  arrange(desc(Totale_Interventi))

#Natura Intervento
tab_7c <- df_mancanti %>%
  group_by(Natura.Intervento) %>%
  summarise(
    Totale_Interventi = n()
  ) %>%
  mutate(
    Perc_Interventi = round((Totale_Interventi / sum(Totale_Interventi)) * 100, 2)
  ) %>%
  select(Natura.Intervento, Totale_Interventi, Perc_Interventi) %>%
  arrange(desc(Totale_Interventi))

################################################################################

tabella1c <- list(
  "Settore" = tab_1c %>% rename(Categoria = 1),
  "Beneficiario" = tab_2c %>% rename(Categoria = 1),
  "Categoria" = tab_3c %>% rename(Categoria = 1),
  "Tipo_PA" = tab_4c %>% rename(Categoria = 1)
) %>% 
  bind_rows(.id = "Nome variabile") %>% 
  rename(n = Procedure_Uniche, `%` = Perc_Procedure)
tabella2c <- list(
  "Anno" = tab_5c %>% rename(Categoria = 1) %>% mutate(Categoria = as.character(Categoria)),
  "Tipo di Intervento" = tab_6c %>% rename(Categoria = 1),
  "Natura Intervento" = tab_7c %>% rename(Categoria = 1)
) %>% 
  bind_rows(.id = "Nome variabile") %>% 
  rename(n = Totale_Interventi, `%` = Perc_Interventi)

write_xlsx(
  list("Overall"=tab_0c , "Distribuzione Interventi"= tab_procedure_interventi_c ,"Procedure" = tabella1c, "Interventi" = tabella2c), 
  "output/Report_mancanti.xlsx"
)

################################################################################
#                         UNITE A CONFRONTO
################################################################################

#procedure
procedure_confronto <- tabella1 %>% 
  rename(n_430 = n, `%_430` = `%`) %>%
  full_join(
    tabella1b %>% rename(n_357 = n, `%_357` = `%`), 
    by = c("Nome variabile", "Categoria")
  ) %>%
  full_join(
    tabella1c %>% rename(n_mancanti = n, `%_mancanti` = `%`), 
    by = c("Nome variabile", "Categoria")
  ) %>%
  mutate(across(everything(), as.character)) %>%
  mutate(across(everything(), ~ replace_na(., "-"))) %>%
  mutate(`Nome variabile` = factor(`Nome variabile`, 
                                   levels = c("Settore", "Beneficiario", "Tipo_PA", "Categoria"))) %>%
  arrange(`Nome variabile`, desc(as.numeric(ifelse(n_430 == "-", 0, n_430))))

#Interventi
interventi_confronto <- tabella2 %>% 
  rename(n_430 = n, `%_430` = `%`) %>%
  full_join(
    tabella2b %>% rename(n_357 = n, `%_357` = `%`), 
    by = c("Nome variabile", "Categoria")
  ) %>%
  full_join(
    tabella2c %>% rename(n_mancanti = n, `%_mancanti` = `%`), 
    by = c("Nome variabile", "Categoria")
  ) %>%
  mutate(across(everything(), as.character)) %>%
  mutate(across(everything(), ~ replace_na(., "-"))) %>%
  mutate(`Nome variabile` = factor(`Nome variabile`, 
                                   levels = c("Anno", "Tipo di Intervento", "Natura Intervento"))) %>%
  arrange(`Nome variabile`, desc(as.numeric(ifelse(n_430 == "-", 0, n_430))))

#Distribuzione interventi
distribuzione_confronto <- tab_procedure_interventi_a %>% 
  rename(n_430 = n, `%_430` = `%`) %>%
  full_join(
    tab_procedure_interventi_b %>% rename(n_357 = n, `%_357` = `%`), 
    by = "Categoria"
  ) %>%
  full_join(
    tab_procedure_interventi_c %>% rename(n_mancanti = n, `%_mancanti` = `%`), 
    by = "Categoria"
  ) %>%
  mutate(across(everything(), as.character)) %>%
  mutate(across(everything(), ~ replace_na(., "-"))) %>%
  arrange(Categoria)

write_xlsx(
  list(
    "Distribuzione Interventi" = distribuzione_confronto,
    "Procedure Complete" = procedure_confronto,
    "Interventi Complete" = interventi_confronto
  ), 
  "output/Report_Confronto_Completo.xlsx"
)


write_xlsx(df_357_completo, "Dataset_357_long.xlsx")
