library(shinyWidgets)
library(shinythemes)
library(shinyreveal)
devtools::load_all("~/r/packages/shinyglide/")

css <- "
.tabtext {
    margin-top: 1em;
}
"

selectize_options <- list(selectOnTab = TRUE, openOnFocus = FALSE, maxOptions = 100)
selectize_options_multi <- c(selectize_options, list(plugins = list("remove_button")))

ui <- fluidPage(
    title = "Prénoms 1900-2017",
    header = tags$head(
        tags$style(css)
    ),
    fluidRow(
        column(8, offset = 2,
            titlePanel("Prénoms 1900-2017"),
            glide(
                height = "600px",
                next_label = "Suivant",
                previous_label = "Précédent",
                disable_type = "disable",
                screen(
                    next_condition = "input.prenom != ''",
                    selectizeInput("prenom",
                        "Prénom :",
                        choices = NULL,
                        multiple = FALSE,
                        options = selectize_options)
                ),
                screenOutput(
                    "screen_test_prenom"
                ),
                screenOutput("screen_sexe"),
                screenOutput("screen_similaires"),
                screen(
                    sliderInput("periode",
                        "Période :",
                        min = 1900,
                        max = 2017,
                        value = c(1900,2017),
                        step = 1,
                        sep = "",
                        width = "100%")
                ),
                screenOutput(
                    "screen_test_prenoms_periode",
                    next_condition = '1 == 2'
                ),
                screen(
                    htmlOutput("texte_pop"),
                    prettyRadioButtons(
                        inputId = "tab_pop_type",
                        label = "", 
                        choices = c("Les deux sexes" = "both", "Seulement les garçons" = "M", "Seulement les filles" = "F"),
                        inline = TRUE, 
                        status = "info",
                        animation = "jelly",
                        fill = TRUE
                    ),
                    tableOutput("tab_pop")
                ),
                screen(
                    htmlOutput("texte_evo"),
                    prettyRadioButtons(
                        inputId = "graph_evo_type",
                        label = "", 
                        choices = c("Nombre de naissances" = "n", "Pourcentage des naissances" = "prop"),
                        inline = TRUE, 
                        status = "info",
                        animation = "jelly",
                        fill = TRUE
                    ),
                    g2Output("graph_evo")
                ),
                screen(
                    htmlOutput("texte_carte"),
                    leafletOutput("graph_carte")
                )
            )
        )
    )
)
                
    
    # 
    # tabPanel("Caractéristique d'un prénom", 
    #     sidebarLayout(
    #             uiOutput("ui_sexe_filter"),
    #             sliderInput("prenom_annees",
    #                 "Période :",
    #                 min = 1900,
    #                 max = 2017,
    #                 value = c(1900,2017),
    #                 step = 1)
    #         ),
    #         
    #         mainPanel(
    #             tabsetPanel(
    #                 tabPanel("Répartition géographique",
    #                     htmlOutput(class = "tabtext", "leafletText"),
    #                     leafletOutput("leafletPlot", height = 800)
    #                 ),
    #                 tabPanel("Évolution dans le temps",
    #                     htmlOutput(class = "tabtext", "evo1Text"),
    #                     plotOutput("evo1Plot", height = 600)
    #                 )
    #             )
    #         )
    #     )
    # ),
    # 
    # tabPanel("Comparaison de prénoms",
    #     sidebarLayout(
    #         sidebarPanel(
    #             selectizeInput("prenoms",
    #                 "Prénom(s) :",
    #                 choices = NULL,
    #                 multiple = TRUE,
    #                 options = selectize_options_multi),
    #             sliderInput("prenoms_annees",
    #                 "Période :",
    #                 min = 1900,
    #                 max = 2017,
    #                 value = c(1900,2017),
    #                 step = 1)
    #         ),
    #         
    #         mainPanel(
    #             tabsetPanel(
    #                 tabPanel("Répartition géographique",
    #                     htmlOutput(class = "tabtext", "dptText"),
    #                     plotOutput("dptPlot", height = 800)
    #                 ),
    #                 tabPanel("Évolution dans le temps",
    #                     htmlOutput(class = "tabtext", "evoText"),
    #                     plotOutput("evoPlot", height = 600)
    #                 )
    #             )
    #         )
    #     )
    # ),
    # 
    # tabPanel("À propos",
    #     p("Départements 2017"),
    #     p("Données nationales pour comparaison"),
    #     p("Source : https://www.insee.fr/fr/statistiques/2540004"),
    #     p("Accents"),
    #     p("")
    # )



