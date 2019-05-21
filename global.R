library(tidyverse)
library(rlang)
library(leaflet)
library(sf)
library(g2r)

load(file = "data/prenoms_2017.Rdata")
load(file = "data/departements_2017.Rdata")


leaflet_dpt <- function(data) {
  
  data <- departements %>% 
    left_join(data, by = "dpt") %>% 
    mutate(label = paste0("<strong>", nom, "</strong><br />",
      "Nombre de naissances : ", nb, "<br />",
      "Pourcentage des naissances : ", round(prop, 1), "%<br />"))

  pal <- colorNumeric("YlGnBu", data$prop)
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


compare_dpt <- function(data, periode) {
  
  data <- departements %>% 
    left_join(data, by = "dpt")
  
  ggplot(data) + 
    geom_sf(aes(fill = prop), color = "white", size = 0.2) +
    scale_fill_viridis_c("Pourcentage\ndes naissances") + 
    ggtitle(paste("Répartition des naissances de", min(periode), "à", max(periode))) +
    theme_minimal() +
    theme(axis.text.x = element_blank(),
      axis.text.y = element_blank()) +
    facet_wrap(~prenom)
  
}


graphe_evo <- function(data, periode) {
  
  n_prenoms <- n_distinct(data$prenom)
  
  g <- ggplot(data) + 
    geom_line(aes(x = annee, y = n, color = prenom)) +
    scale_color_brewer("Prénom", palette = "Set1") +
    scale_x_continuous("Année", breaks = seq(min(periode), max(periode), 10)) +
    scale_y_continuous("Nombre de naissances", limits = c(0,NA))
  
}