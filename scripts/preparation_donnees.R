
## Préparation données -----------------------------

# devtools::install_github("antuki/CARTElette/CARTElette@RPackage")
library(CARTElette)
departements_orig <- loadMap(COG = 2017, nivsupra = "DEP")
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



save(departements, file = "data/departements_2017.Rdata")


data_dpt <- read_tsv("data/raw/dpt2017.txt")
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

data_nat <- read_tsv("data/raw/nat2017.txt")
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
  filter(nchar(prenom) > 1)

liste_prenoms <- unique(data_nat$prenom)
liste_prenoms <- liste_prenoms[liste_prenoms != "_PRENOMS_RARES"]
liste_prenoms_ascii <- iconv(liste_prenoms, to='ASCII//TRANSLIT')

save(data_dpt, data_nat, liste_prenoms, liste_prenoms_ascii, file = "data/prenoms_2017.Rdata")

