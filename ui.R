library(shinyWidgets)
library(shinythemes)
library(shinyglide)
library(shinycssloaders)

options(spinner.type = 7)
options(spinner.color = "#999999")

controls <- glideControls(
    prevButton(),
    list(
        nextButton(),
        lastButton(
            class="btn btn-success",
            href="https://juba.github.io/shinyglide",
            "Go to project website"
        )
    )
)


ui <- fluidPage(
    title = "Prénoms 1900-2017",
    theme = "shiny_prenoms.css",
    fluidRow(
        div(class="glide-wrapper",
            glide(
                height = "650px",
                next_label = paste(
                    "Suivant",
                    shiny::icon("chevron-right", lib = "glyphicon")
                ),
                previous_label = paste(
                    shiny::icon("chevron-left", lib = "glyphicon"),
                    "Précédent"
                ),
                loading_label = span(
                    span(class = "shinyglide-spinner"),
                    span("Chargement")
                ),
                disable_type = "disable",
                custom_controls = controls,
                
                screen(
                    h1("Prénoms donnés à la naissance en France"),
                    h2("1900-2017")
                ),
                screen(
                    next_condition = "input.prenom != ''",
                    h3("Choisissez un prénom et une période"),
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
                        width = "90%")
                ),
                screenOutput("screen_affine_prenom"),
                screen(
                    h4(htmlOutput("legende_evo")),
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
                    h4(htmlOutput("legende_pop")),
                    div(class="center-block",
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
                    )
                ),
                screen(
                    p(htmlOutput("legende_carte")),
                    withSpinner(leafletOutput("graph_carte"))
                ),
                screen(
                    p("Que souhaitez-vous faire ?"),
                    div(`data-glide-el`="controls",
                        tags$button(`data-glide-dir`="=1", "Recommencer"),
                        tags$button(`data-glide-dir`=">", "Comparer")
                    )
                ),
                screen(
                    next_condition = "input.prenoms_comp != ''",
                    p("Comparer des prénoms"),
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
                    ),
                    fluidRow(
                        column(7,
                            selectizeInput("prenoms_comp4",
                                "Prénoms :",
                                choices = NULL,
                                multiple = TRUE,
                                options = selectize_options_multi)
                        ),
                        column(5,
                            selectInput("sexe_comp4",
                                "Filter sur le sexe :",
                                choices = select_sexe_choices)
                        )
                    ),
                    sliderInput("periode_comp",
                        "Période :",
                        min = 1900,
                        max = 2017,
                        value = c(1900,2017),
                        step = 1,
                        sep = "",
                        width = "90%")
                ),
                screen(
                    p(htmlOutput("legende_evo_comp")),
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
                    p(htmlOutput("legende_carte_comp")),
                    withSpinner(leafletOutput("graph_carte_comp"))
                )
            )
        )
    )
)





