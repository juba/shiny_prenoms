library(tidyverse)
library(rlang)
library(leaflet)
library(sf)
library(g2r)
library(glue)
library(data.table)

load(file = "data/prenoms_2017.Rdata")
load(file = "data/departements_2017.Rdata")

data_nat <- data.table(data_nat, key = "prenom")
data_dpt <- data.table(data_dpt, key = "prenom")

selectize_options <- list(selectOnTab = TRUE, openOnFocus = FALSE, maxOptions = 100)
selectize_options_multi <- c(selectize_options, list(plugins = list("remove_button")))

select_sexe_choices <- c("Garder les deux sexes" = "both", 
  "Seulement les garçons" = "M", 
  "Seulement les filles" = "F")


highlight_options <- highlightOptions(color = "#F99800", weight = 2,
  bringToFront = TRUE)
label_options <- list(opacity = 0.9, offset = c(0, -40), direction = "top")


leaflet_dpt <- function(data) {
  
  data <- departements %>% 
    left_join(data, by = "dpt") %>% 
    mutate(
      label_pourcentage = ifelse(!is.na(`%`), glue("Pourcentage des naissances : {round(`%`, 3)}%<br />"), ""),
      label = glue("<strong>{nom}</strong><br />Nombre de naissances : {n}<br />{label_pourcentage}")
    )

  domain <- if (all(is.na(data$`%`))) NULL else data$`%`
  pal <- colorNumeric("PuRd", domain)
  
  leaflet(data) %>% 
    addPolygons(label = ~purrr::map(label, htmltools::HTML),
      labelOptions = label_options,
      color = "#444444", weight = 1, smoothFactor = 1,
      opacity = 1.0, fillOpacity = 0.9,
      fillColor = ~pal(`%`),
      highlightOptions =  highlight_options) %>%
    addLegend("topright",
      title = "Pourcentage des<br>naissances dans<br>le département",
      pal = pal, values = data$`%`,
      bins = 6,
      opacity = 1,
      na.label = "NA",
      labFormat = labelFormat(
        suffix = "%",
        digits = 4
      )
    )

}

leaflet_dpt_comp <- function(data) {

  datas <- data %>% 
    group_by(prenom) %>% 
    group_split()
  keys <- data %>% 
    group_by(prenom) %>% 
    group_keys() %>% 
    pull(1)
  
  datas <- purrr::map2(datas, keys, function(data, key) {
    departements %>% 
      left_join(data, by = "dpt") %>% 
      mutate(prenom = ifelse(is.na(prenom), key, prenom)) %>% 
      mutate(label = paste0("<strong>", nom, "</strong><br />",
        "Nombre de naissances : ", n, "<br />",
        "Pourcentage des naissances : ", round(`%`, 3), "%<br />"))
  })
  
  domain <- if (all(is.na(data$`%`))) NULL else data$`%`
  pal <- colorNumeric("PuRd", domain)

  map <- leaflet()
  
  for(i in seq_along(datas)) {
    map <- map %>% 
      addPolygons(data = datas[[i]], 
        group = keys[i],
        label = ~purrr::map(label, htmltools::HTML),
        labelOptions = label_options,
        color = "#444444", weight = 1, smoothFactor = 1,
        opacity = 1.0, fillOpacity = 0.9,
        fillColor = ~pal(`%`),
        highlightOptions =  highlight_options)
  }
  
  map <- map %>% 
    addLegend("topright",
      title = "Pourcentage des<br>naissances dans<br>le département",
      pal = pal, values = data$`%`,
      bins = 4,
      opacity = 1,
      na.label = "NA",
      labFormat = labelFormat(
        suffix = "%",
        digits = 4
      )
    ) %>% 
    addLayersControl(
      baseGroups = as.character(keys),
      options = layersControlOptions(collapsed = FALSE)
    )
  
  map  

}





# compare_dpt <- function(data, periode) {
#   
#   data <- departements %>% 
#     left_join(data, by = "dpt")
#   
#   ggplot(data) + 
#     geom_sf(aes(fill = prop), color = "white", size = 0.2) +
#     scale_fill_viridis_c("Pourcentage\ndes naissances") + 
#     ggtitle(paste("Répartition des naissances de", min(periode), "à", max(periode))) +
#     theme_minimal() +
#     theme(axis.text.x = element_blank(),
#       axis.text.y = element_blank()) +
#     facet_wrap(~prenom)
#   
# }

