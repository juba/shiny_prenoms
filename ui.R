library(shinyWidgets)
library(shinythemes)
library(shinyglide)
library(shinycssloaders)
devtools::load_all("~/r/packages/shinyglide/")

options(spinner.type = 7)
options(spinner.color = "#999999")


ui <- fluidPage(
    title = "Prénoms 1900-2017",
    theme = "shiny_prenoms.css",
    fluidRow(
        column(8, offset = 2,
            glide(
                height = "600px",
                next_label = 'Suivant <span class="glyphicon glyphicon-chevron-right" aria-hidden="true"></span>',
                previous_label = '<span class="glyphicon glyphicon-chevron-left" aria-hidden="true"></span> Précédent',
                loading_label = "Chargement",
                disable_type = "disable",
                
                screen(
                    h2("Prénoms donnés à la naissance en France"),
                    h3("1900-2017")
                ),
                screen(
                    next_condition = "input.prenom != ''",
                    selectizeInput("prenom",
                        "Prénom :",
                        choices = NULL,
                        multiple = FALSE,
                        options = selectize_options),
                    sliderInput("periode",
                        "Période :",
                        min = 1900,
                        max = 2017,
                        value = c(1900,2017),
                        step = 1,
                        sep = "",
                        width = "100%")
                ),
                screenOutput("screen_sexe_similaires"),
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
                    withSpinner(tableOutput("tab_pop"))
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
                    withSpinner(g2Output("graph_evo"))
                ),
                screen(
                    htmlOutput("texte_carte"),
                    withSpinner((leafletOutput("graph_carte"))
                    ),
                    screen(
                        p("Que souhaitez-vous faire ?"),
                        div(`data-glide-el`="controls",
                            tags$button(`data-glide-dir`="={1}", "Recommencer"),
                            tags$button(`data-glide-dir`=">", "Comparer")
                        )
                        ),
                    screen(
                        next_condition = "input.prenoms_comp != ''",
                        p("Comparer avec d'autres prénoms"),
                        fluidRow(
                            column(7,
                                selectizeInput("prenoms_comp1",
                                    "Prénoms :",
                                    choices = NULL,
                                    multiple = TRUE,
                                    options = selectize_options_multi)
                                ),
                            column(5,
                                selectInput("sexe_comp1",
                                    "Filter sur le sexe :",
                                    choices = select_sexe_choices)
                            )
                        ),
                        fluidRow(
                            column(7,
                                selectizeInput("prenoms_comp2",
                                    "Prénoms :",
                                    choices = NULL,
                                    multiple = TRUE,
                                    options = selectize_options_multi)
                            ),
                            column(5,
                                selectInput("sexe_comp2",
                                    "Filter sur le sexe :",
                                    choices = select_sexe_choices)
                            )
                        ),
                        fluidRow(
                            column(7,
                                selectizeInput("prenoms_comp3",
                                    "Prénoms :",
                                    choices = NULL,
                                    multiple = TRUE,
                                    options = selectize_options_multi)
                            ),
                            column(5,
                                selectInput("sexe_comp3",
                                    "Filter sur le sexe :",
                                    choices = select_sexe_choices)
                            )
                        )
                    ),
                    screen(
                        htmlOutput("texte_evo_comp"),
                        prettyRadioButtons(
                            inputId = "graph_evo_comp_type",
                            label = "", 
                            choices = c("Nombre de naissances" = "n", "Pourcentage des naissances" = "prop"),
                            inline = TRUE, 
                            status = "info",
                            animation = "jelly",
                            fill = TRUE
                        ),
                        withSpinner(g2Output("graph_evo_comp"))
                    ),
                    screen(
                        htmlOutput("texte_carte_comp"),
                        withSpinner(leafletOutput("graph_carte_comp"))
                    )
                )
            )
        )
    )
)




