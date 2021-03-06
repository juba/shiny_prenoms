
## Préparation données -----------------------------
library(tidyverse)

# devtools::install_github("antuki/CARTElette/CARTElette@RPackage")
library(CARTElette)
departements_orig <- charger_carte(COG = 2019, nivsupra = "DEP")
departements <- departements_orig %>% rename(dpt = DEP)
## Fusion de la Corse
library(sf)
library(rmapshaper)
departements <- departements %>% 
  mutate(dpt = if_else(dpt %in% c("2A", "2B"), "20", dpt)) %>% 
  filter(dpt != "976") %>% 
  group_by(dpt) %>% 
  summarise(nom = first(nom), geometry = st_union(geometry)) %>% 
  rmapshaper::ms_simplify(keep = 0.03) %>% 
  st_transform(crs = 4326)

save(departements, file = "data/departements_2019.Rdata")

## Données prénoms départementales
data_dpt <- read_csv2("data/raw/dpt2019.csv")
data_dpt <- data_dpt %>% 
  rename(prenom = preusuel,
    annee = annais,
    n = nombre) %>% 
  filter(annee != "XXXX") %>% 
  mutate(annee = as.numeric(annee)) %>% 
  mutate(
    sexe = as.character(sexe),
    sexe = fct_recode(sexe,
           "M" = "1",
           "F" = "2"),
    sexe = as.character(sexe))

## Données prénoms nationales
data_nat <- read_csv2("data/raw/nat2019.csv")
data_nat <- data_nat %>% 
  rename(prenom = preusuel,
    annee = annais,
    n = nombre) %>% 
  filter(annee != "XXXX") %>% 
  mutate(annee = as.numeric(annee)) %>% 
  mutate(
    sexe = as.character(sexe),
    sexe = fct_recode(sexe,
      "M" = "1",
      "F" = "2"),
    sexe = as.character(sexe)) %>% 
  filter(nchar(prenom) > 1) %>% 
  group_by(annee) %>% 
  mutate(n_annee = sum(n)) %>% 
  ungroup()

liste_prenoms <- unique(data_nat$prenom)
liste_prenoms <- liste_prenoms[liste_prenoms != "_PRENOMS_RARES"]

## Conversion ASCII
liste_prenoms_ascii <- iconv(liste_prenoms, to='ASCII//TRANSLIT')
## Suppression des lettres en double
liste_prenoms_ascii <- str_replace_all(liste_prenoms_ascii, "(.)\\1", "\\1")

sexes_prenoms <- data_nat %>% 
  count(prenom, sexe) %>% 
  count(prenom)

save(data_dpt, data_nat, liste_prenoms, sexes_prenoms, liste_prenoms_ascii, file = "data/prenoms_2019.Rdata")

