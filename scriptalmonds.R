library(ggplot2)
library(dplyr)
library(tidyr)


### PARTE 1: IMPORTAZIONE DEL DATASET ####

# Titolo: Pollination dependency and deficit in three almond varietes from Morocco
# Autori: Abdessamad Aglagane, Ayyoub Skaou
# Anno: 2024
# https://doi.org/10.5061/dryad.msbcc2g66
# Descrizione: il dataset utilizzato contiene informazioni sulla qualità dei frutti 
# di mandorlo di diverse varietà (Largueta, Espoir e Planeta)
# in relazione a diversi trattamenti di impollinazione:
# - Open: impollinazione naturale
# - Open+HP: impollinazione naturale più impollinazione manuale 
# - Bagged:  con frutti protetti da una retina per evitare l'impollinazione naturale
# I dati sono stati raccolti al fine di valutare il contributo dell'impollinazione 
# entomofila alla qualità dei frutti di mandorlo.
# Di seguito vengono analizzati i dati del secondo foglio "Fruit_quality".

# Setto la working directory e importo il dataset
setwd("C:/Users/linda/Desktop/Almonds")
getwd() # per controllare che la working directory sia corretta

dati_raw <- read.csv2("data/data_raw/Fruit_quality.CSV") 

dati_raw %>% 
  head() # per visualizzare le prime righe del dataset
str(dati_raw) # per visualizzare la struttura del dataset e i tipi di variabili
dim(dati_raw) # per visualizzare il numero di righe e colonne del dataset
names(dati_raw) # per visualizzare i nomi delle colonne del dataset

unique(dati_raw$Varieties) # per visualizzare le categorie uniche presenti nella variabile Varieties
unique(dati_raw$Treatments)

### PARTE 2: CONTROLLO QUALITÀ #### 

# per controllare se ci sono NA
colSums(is.na(dati_raw))

# conto righe e colonne
nrow(dati_raw)
ncol(dati_raw)

# per trasformare character in factor e numeric e rinominare le colonne
dati_raw <- dati_raw %>%
  transmute(             # uso transmute per modificare colonne già esistenti
    Varieta = as.factor(Varieties), # ed eliminare quelle vecchie
    Trattamento = as.factor(Treatments),
    Pericarpo_g = as.numeric(Pericarp_g),
    Seme_g = as.numeric(Kernel_g),
    Endocarpo_g = as.numeric(Endocarp_g)
  )
  
str(dati_raw)

# conteggio per categorie
dati_raw %>% 
  group_by(Trattamento) %>%
  summarise(count = n())

dati_raw %>% 
  group_by(Varieta) %>%
  summarise(count = n())

dati_raw %>% 
  group_by(Varieta, Trattamento) %>%
  summarise(count = n())

### PARTE 3: PULIZIA ####

# il dataset non contiene NA né variabili non utili 
# se pericarpo, endocarpo e kernel sono tutti e tre pari a 0,
# significa che il frutto in quel caso non si è sviluppato

# il trattamento "bagged" ha una sola osservazione per varietà

data_clean <- dati_raw


### PARTE 4: NUOVE VARIABILI ####

# aggiungo le variabili numeriche peso totale e resa del seme
# peso totale = somma di pericarpo, endocarpo e seme
# resa del seme = peso del seme / peso totale * 100
data_new <- data_clean %>%
  mutate(Peso_tot = Pericarpo_g + Endocarpo_g + Seme_g,
         Resa = Seme_g / Peso_tot * 100) 

str(data_new)
head(data_new)

# per visualizzare il dataset ordinato per Peso_tot crescente
data_new %>% 
  arrange(Peso_tot) 


# creo variabile categorica per il peso totale del frutto basata sui quartili
# aggiungo la nuova colonna con mutate 
# e case_when per definire le classi di peso del frutto in base ai quartili
quartili_peso <- quantile(data_new$Peso_tot, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
quartili_peso

data_new <- data_new %>% 
  mutate(Peso_tot_classe = case_when(
    Peso_tot <= quartili_peso[1] ~ "very low",
    Peso_tot > quartili_peso[1] & Peso_tot <= quartili_peso[2] ~ "low",
    Peso_tot > quartili_peso[2] & Peso_tot <= quartili_peso[3] ~ "high",
    Peso_tot > quartili_peso[3] ~ "very high"
  ))


str(data_new)
head(data_new)

### PARTE 5: ANALISI DESCRITTIVA ####

# considerando solo i frutti sviluppati (Peso_tot > 0), 
# calcolo il numero di osservazioni, media, deviazione standard, 
# minimo e massimo per peso totale e resa del seme

# qui calcolo delle statistiche descrittive

summary <- data_new %>%
  filter(Peso_tot > 0) %>% # considero solo le righe con i frutti sviluppati
  select(Varieta, Trattamento, Peso_tot, Resa) %>% # seleziono solo le colonne di interesse
  group_by(Varieta, Trattamento) %>% 
  summarise(
    count = n(), # numero di osservazioni per ogni combinazione di Varieta e Trattamento
    across(c(Peso_tot, Resa), # calcolo media, sd, min e max per le due colonne Peso_tot e Resa
           list(mean = ~ mean(.x, na.rm = TRUE), # lista di funzioni da applicare a ciascuna colonna
                sd = ~ sd(.x, na.rm = TRUE),
                min = ~ min(.x, na.rm = TRUE),
                max = ~ max(.x, na.rm = TRUE)))
  )

summary 

# i NA sono dovuti al fatto che per la varietà Espoir 
# con trattamento Bagged si è sviluppato un solo frutto
# dunque non è possibile calcolare deviazione standard


### PARTE 6: PIVOT ####


### formato wide ####

fruit_wide <- data_new %>%
  select(Varieta, Trattamento, Peso_tot, Seme_g) 

### formato long ####

# data_new originale è già in formato wide

# il formato long in questo caso permette di confrontare più facilmente
# in ggplot in un istogramma tre diverse misure per ogni varietà
# misure che nel formato wide sono presenti in 3 colonne diverse
# che quindi richiederebbero tre grafici diversi
# in questo senso il formato long è più leggibile
# per ggplot, anche se meno intuitivo 

fruit_long <- data_new %>%
  select(Varieta, Trattamento, Pericarpo_g, Endocarpo_g, Seme_g) %>%
  pivot_longer(
    cols = c(Pericarpo_g, Endocarpo_g, Seme_g), # colonne da trasformare in formato long
    names_to = "Componente", # nome della nuova colonna con le componenti del frutto
    values_to = "Peso_comp" # nome della nuova colonna con i valori dei pesi delle componenti
  ) %>% 
  mutate(Componente = factor(Componente, # per ordinare le componenti del frutto 
                            levels = c("Pericarpo_g", "Endocarpo_g", "Seme_g"), 
                            labels = c("Pericarpo", "Endocarpo", "Seme")))



# per tornare al formato wide devo assegnare un ID univoco a ogni riga (quindi a ogni frutto)
# prima di pivotare di nuovo, altrimenti pivot_wider 
# non sa come ricostruire le righe originali
# e restituirebbe una lista di valori per ogni misura
# senza riconoscere a quale frutto appartengono

fruit_long_2 <- data_new %>%
  mutate(frutto_id = row_number()) %>% # creo un numero unico per ogni frutto
  pivot_longer(
    cols = c(Pericarpo_g, Endocarpo_g, Seme_g), 
    names_to = "Componente", 
    values_to = "Peso_comp" 
  )

fruit_wide2 <- fruit_long_2 %>% 
  pivot_wider(
    names_from = Componente, # prendo i nomi dalla colonna Componente
    values_from = Peso_comp # prendo i valori dalla colonna Peso_comp
  )

# Con fruit_wide2, avrei bisogno di creare tre grafici per visualizzare
# il peso delle diverse componenti del frutto per varietà,
# mentre con fruit_long posso visualizzarle tutte insieme;
# nomi e valori distribuiti in tre colonne per il formato wide;
# due colonne per il formato long: nome e valori


### PARTE 7: GRAFICI ####

### peso totale vs peso del seme ####
# RELAZIONE PESO DEL FRUTTO E PESO DEL SEME
# Tendenzialmente, il peso del seme aumenta all'aumentare del peso totale

ggplot(fruit_wide, aes(x = Peso_tot, y = Seme_g)) +
  geom_point(aes(color = Varieta, shape = Trattamento), # aggiungo colore e forma per distinguere le varietà e i trattamenti
             size = 1.5, alpha= 0.5) + # dimensione e trasparenza dei punti per migliorare la leggibilità
  labs(                                # etichette
    title = "Relazione tra peso del frutto e del seme",
    x = "Peso del frutto (g)",
    y = "Peso del seme (g)",
    color = "Varietà",
    shape = "Trattamento"
  ) +
  theme_minimal(base_size = 12) + # dimensione base del testo più grande
  theme(
    plot.title = element_text(hjust = 0.5, margin = margin(b = 15)), # spazio sotto il titolo
    axis.title.x = element_text(margin = margin(t = 15)), # spazio sopra l'etichetta X (allontana dal grafico)
    axis.title.y = element_text(margin = margin(r = 15))  # spazio a destra dell'etichetta Y (allontana dal grafico)
  )
 

### classi di peso del frutto ####
# DISTRIBUZIONE DELLE OSSERVAZIONI PER CLASSE DI PESO DEL FRUTTO PER VARIETÀ
# conto il numero di osservazioni per ogni combinazione di varietà e classe di peso del frutto
# ordino le classi di peso in modo logico (very low, low, high, very high) 
# per una miglior visualizzazione 

# Espoir ha il maggior numero di frutti con peso basso o molto basso
# Largueta ha il maggior numero di frutti con peso molto alto
# Planeta ha una distribuzione più equilibrata tra le classi di peso del frutto

count_peso_tot <- data_new %>%
  group_by(Varieta, Peso_tot_classe) %>%
  summarise(n = n(), .groups = "drop") %>% 
  mutate( # drop serve a evitare che mutate non avvenga per gruppo, ma per tutto il dataset
    Peso_tot_classe = factor(Peso_tot_classe, c("very low", "low", "high", "very high"))
  )

head(count_peso_tot)

ggplot(count_peso_tot, aes(x = Peso_tot_classe, y = n, fill = Peso_tot_classe)) +
  geom_col(show.legend = FALSE) + # per evitare una legenda ridondante
  facet_wrap(~ Varieta) + # per creare un grafico separato per ogni varietà
  labs(
    title = "Distribuzione osservazioni per classe di peso del frutto",
    x = "Classe di peso",
    y = "Numero di osservazioni"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, margin = margin(b = 15)), # spazio sotto il titolo
    axis.title.x = element_text(margin = margin(t = 15)), # spazio sopra l'etichetta X (allontana dal grafico)
    axis.title.y = element_text(margin = margin(r = 15))  # spazio a destra dell'etichetta Y (allontana dal grafico)
  )

### peso vs varietà e trattamento ####
# PESO DEL FRUTTO RISPETTO ALLA VARIETA E AL TRATTAMENTO
# Bagged ha solo 3 osservazioni, una per Varieta, e solo Espoir produce frutto
# Open produce pesi dei frutti molto variabili, 
# con marcate differenze con Open+HP solo nella Varieta Largueta
# open e Open+HP sono le solo ad avere numero di osservazioni confrontabile
# Open produce pesi più variabili, e con una media più bassa rispetto a Open+HP


ggplot(fruit_wide, aes(x = Varieta, y = Peso_tot, fill = Trattamento)) +
  geom_boxplot() +
    labs(
    title = "Peso del frutto rispetto alla varietà e al trattamento",
    x = "Varietà",
    y = "Peso del frutto (g)",
    fill = "Trattamento") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, margin = margin(b = 15)), # spazio sotto il titolo
    axis.title.x = element_text(margin = margin(t = 15)), # spazio sopra l'etichetta X (allontana dal grafico)
    axis.title.y = element_text(margin = margin(r = 15))  # spazio a destra dell'etichetta Y (allontana dal grafico)
  )


### resa vs varietà e trattamento ####
# RESA DEL SEME RISPETTO ALLA VARIETA E AL TRATTAMENTO
# Largueta ha mediamente rese minori rispetto alle altre Varieta
# il trattamento Open ha sempre rese più variabili

data_new %>% 
  filter(Peso_tot > 0) %>%
  ggplot(aes(x = Varieta, y = Resa, fill = Trattamento)) +
  geom_boxplot() +
  labs(
    title = "Resa del seme rispetto alla varietà e al trattamento",
    x = "Varietà",
    y = "Resa del seme (%)",
    fill = "Trattamento") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, margin = margin(b = 15)), 
    axis.title.x = element_text(margin = margin(t = 15)), 
    axis.title.y = element_text(margin = margin(r = 15)) 
    )


### biomassa vs varietà ####
# ALLOCAZIONE DELLA BIOMASSA NEL FRUTTO RISPETTO ALLA VARIETA E AL TRATTAMENTO
# Calcolo il peso medio di ogni componente del frutto (Pericarpo, Endocarpo, Seme) 
# per ogni combinazione di Varieta e Trattamento

#Largueta produce frutti piu pesanti, con endocarpo più pesante
# Espoir produce frutti più leggeri

peso_summary_plot <- fruit_long %>%
  group_by(Varieta, Componente) %>%
  summarise(Peso_medio = mean(Peso_comp, na.rm = TRUE), # calcolo media del peso di ogni componente 
            .groups = "drop")


ggplot(peso_summary_plot, aes(x = Varieta, y = Peso_medio, fill = Componente)) +
  geom_col(position = "dodge") + # dodge per affiancare le barre delle componenti del frutto invece di sovrapporle
  labs(
    title = "Peso medio dei componenti del frutto per varietà",
    x = "Varietà",
    y = "Peso medio (g)",
    fill = "Componente"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10))
  )

### biomassa vs trattamento ####

# Bagged ha una sola osservazione (non c'è valore medio)
# Open+HP ha frutti più pesanti, ma con rese simili a Open
peso_summary_plot2 <- fruit_long %>%
  group_by(Trattamento, Componente) %>%
  summarise(Peso_medio = mean(Peso_comp, na.rm = TRUE), .groups = "drop")

ggplot(peso_summary_plot2, aes(x = Trattamento, y = Peso_medio, fill = Componente)) +
  geom_col(position = "dodge") +
  labs(
    title = "Allocazione biomassa nel frutto rispetto al trattamento",
    x = "Trattamento",
    y = "Peso medio (g)",
    fill = "Componenti del frutto") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, margin = margin(b = 15)), 
    axis.title.x = element_text(margin = margin(t = 15)), 
    axis.title.y = element_text(margin = margin(r = 15))
  )
  
### PARTE 8: OUTPUT ####

# Tabella pulita
write.csv(data_clean, "data/data_clean/data_clean.csv", row.names = FALSE)

# Tabella riassuntiva
write.csv(summary, "output/summary_statistics.csv", row.names = FALSE)

# Dataset finale con le nuove variabili
saveRDS(data_new, "output/data_final.rds")

# Grafici
ggsave("figures/pTOT_pSEME.png", width = 8, height = 6, dpi = 300)
ggsave("figures/classi_peso_tot.png", width = 8, height = 6, dpi = 300)
ggsave("figures/peso_frutto.png", width = 8, height = 6, dpi = 300)
ggsave("figures/resa_seme.png", width = 8, height = 6, dpi = 300)
ggsave("figures/allocazione_biomassa_varieta.png", width = 8, height = 6, dpi = 300)
ggsave("figures/allocazione_biomassa_trattamento.png", width = 8, height = 6, dpi = 300)




### PARTE 11: GITHUB ####


system('git config --global user.email "linda.frigioni@student.univaq.it"')
system('git config --global user.name "Linda Frigioni"')

# per escludere i file che non devono andare su github
file.create(".gitignore")

writeLines(c(
  ".Rhistory",
  ".RData",
  ".Rproj.user/",
  "data/data_raw/",
  "data/data_clean/"
), ".gitignore")


usethis::use_git() 
usethis::use_github() 

# per rimuovere i file già tracciati da git, ma che ora sono stati aggiunti al .gitignore
system("git rm -r --cached data/data_raw")
system("git rm -r --cached data/data_clean")
# questo comando rimuove i file dalla cache di git, ma li lascia intatti nella cartella locale
system('git commit -m "Remove data from repository (ignored via gitignore)"')

# per fare il commit e push delle modifiche al repository GitHub
system("git push origin master")

system("git status")





