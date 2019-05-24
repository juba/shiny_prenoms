library(tidyverse)
library(rlang)
library(leaflet)
library(sf)
library(g2r)

load(file = "data/prenoms_2017.Rdata")
load(file = "data/departements_2017.Rdata")


selectize_options <- list(selectOnTab = TRUE, openOnFocus = FALSE, maxOptions = 100)
selectize_options_multi <- c(selectize_options, list(plugins = list("remove_button")))
select_sexe_choices <- c("Garder les deux sexes" = "both", 
  "Seulement les garçons" = "M", 
  "Seulement les filles" = "F")

leaflet_dpt <- function(data) {
  
  data <- departements %>% 
    left_join(data, by = "dpt") %>% 
    mutate(label = paste0("<strong>", nom, "</strong><br />",
      "Nombre de naissances : ", nb, "<br />",
      ifelse(!is.na(prop), paste("Pourcentage des naissances : ", 
        round(prop, 3), "%<br />"), "")))

  domain <- if (all(is.na(data$prop))) NULL else data$prop
  pal <- colorNumeric("YlGnBu", domain)
  highlight_options <- highlightOptions(color = "#F99800", weight = 2,
    bringToFront = TRUE)
  label_options <- list(opacity = 0.9, offset = c(0, -40), direction = "top")
  
  leaflet(data) %>% 
    #addProviderTiles(providers$CartoDB.Positron) %>% 
    addPolygons(label = ~purrr::map(label, htmltools::HTML),
      labelOptions = label_options,
      color = "#444444", weight = 1, smoothFactor = 1,
      opacity = 1.0, fillOpacity = 0.9,
      fillColor = ~pal(prop),
      highlightOptions =  highlight_options) %>%
    addLegend("bottomright",
      pal = pal, values = data$prop,
      title = "(en %)",
      opacity = 1,
      na.label = "NA")

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
        "Nombre de naissances : ", nb, "<br />",
        "Pourcentage des naissances : ", round(prop, 3), "%<br />"))
  })
  
  pal <- colorNumeric("YlGnBu", data$prop)
  highlight_options <- highlightOptions(color = "#F99800", weight = 2,
    bringToFront = TRUE)
  label_options <- list(opacity = 0.9, offset = c(0, -40), direction = "top")
  
  map <- leaflet()
  
  for(i in seq_along(datas)) {
    map <- map %>% 
      addPolygons(data = datas[[i]], 
        group = keys[i],
        label = ~purrr::map(label, htmltools::HTML),
        labelOptions = label_options,
        color = "#444444", weight = 1, smoothFactor = 1,
        opacity = 1.0, fillOpacity = 0.9,
        fillColor = ~pal(prop),
        highlightOptions =  highlight_options)
  }
  
  map <- map %>% 
    addLegend("bottomright",
      pal = pal, values = data$prop,
      title = "(en %)",
      opacity = 1,
      na.label = "NA") %>% 
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

