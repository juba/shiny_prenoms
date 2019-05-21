# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
    updateSelectizeInput(session, 'prenom', choices = c(NA, liste_prenoms), server = TRUE)  

    periode <- reactive({
      min <- input$periode[1]
      max <- input$periode[2]
      seq(min, max)
    })
    
    prenom_ok <- reactive({
      req(input$prenom) %in% liste_prenoms
    })
    
    output$screen_test_prenom <- renderUI({
      if (prenom_ok()) return(NULL)
      p("Le prénom choisi n'est pas présent dans la base.")
    })
    outputOptions(output, "screen_test_prenom", suspendWhenHidden = FALSE)
    
    nb_sexe_prenom <- reactive({
      if (req(input$prenom) == "") return(FALSE)
      tmp <- data_nat %>% 
        filter(prenom == input$prenom) %>% 
        summarise(nb = n_distinct(sexe)) %>% 
        pull(nb)
      return(tmp)
    })
    
    output$screen_sexe <- renderUI({
      if (nb_sexe_prenom() < 2) return("")
      selectInput("sexe",
        "Filter sur le sexe :",
        choices = c("Garder les deux sexes" = "both", 
                    "Seulement les garçons" = "M", 
                    "Seulement les filles" = "F"))
    })
    outputOptions(output, "screen_sexe", suspendWhenHidden = FALSE)
    
    sexe_prenom <- reactive({
      if (nb_sexe_prenom() < 2 || req(input$sexe) == "both") return(c("M", "F"))
      input$sexe
    })
    
    prenoms_similaires <- reactive({
      prenom_ascii <- iconv(input$prenom, to='ASCII//TRANSLIT')
      res <- liste_prenoms[liste_prenoms_ascii == prenom_ascii]
      res[res != input$prenom]
    })
    
    prenoms_choisis <- reactive({
      c(input$prenom, input$prenoms_similaires)
    })
    
    output$screen_similaires <- renderUI({
      if (length(prenoms_similaires()) == 0) return(NULL)
      checkboxGroupInput(
        "prenoms_similaires",
        "Regrouper avec",
        choices = prenoms_similaires()
      )
    })
    outputOptions(output, "screen_similaires", suspendWhenHidden = FALSE)
    
    output$screen_test_prenoms_periode <- renderUI({
      tmp <- data_nat %>% 
        filter(
          prenom %in% prenoms_choisis(),
          sexe %in% sexe_prenom(),
          annee %in% periode()
        )
      if (nrow(tmp) > 0) return(NULL)
      p("Le prénom choisi n'existe pas dans la base à cette période.")
    })
    outputOptions(output, "screen_test_prenoms_periode", suspendWhenHidden = FALSE)
    
    
    generate_texte <- function(intro_text) {
      text <- paste(intro_text, "<strong>", input$prenom, "</strong>")
      if (!is.null(input$sexe) && input$sexe != "both") {
        text <- paste0(text, " (sexe = ", input$sexe, ")")
      }
      if (length(periode()) == 1) {
        text <- paste0(text, " en ", periode())
      } else {
        text <- paste0(text, " de ", input$periode[1], " à ", input$periode[2])
      }
      text
    }
    
    output$texte_pop <- renderUI({
      text <- paste0("<p>",
        generate_texte("Popularité de"),
        ".</p>")
      return(HTML(text))
    })
    
    data_pop <- reactive({
      
      if (input$prenom == "") return(NULL)
      
      if (input$tab_pop_type == "M") {
        tmp <- data_nat %>% filter(sexe == "M")
      } else if (input$tab_pop_type == "F") {
        tmp <- data_nat %>% filter(sexe == "F") 
      } else {
        tmp <- data_nat
      }
      
      tmp %>% 
        filter(annee %in% periode()) %>% 
        mutate(n_total = sum(n)) %>% 
        filter(prenom != "_PRENOMS_RARES") %>% 
        mutate(prenom = if_else(prenom %in% prenoms_choisis(), input$prenom, prenom)) %>% 
        group_by(prenom, sexe) %>% 
        summarise(
          n = as.integer(sum(n)),
          n_total = first(n_total),
          `%` = round(n / n_total * 100, 3)
        ) %>% 
        ungroup() %>% 
        arrange(desc(n)) %>% 
        mutate(Classement = 1:n()) %>% 
        select(Classement, Prénom = prenom, Sexe = sexe, Naissances = n, `%`)
    })
    
    output$tab_pop_top <- renderTable({
      data_pop() %>% slice(1:10)
    }, striped = TRUE, hover = TRUE)
    
    output$tab_pop_prenom <- renderTable({
      classement_prenom <- data_pop()$Classement[data_pop()$Prénom == input$prenom]
      if (length(classement_prenom) == 0 || classement_prenom %in% 1:10) return(NULL)
      data_pop() %>% slice(classement_prenom)
    }, striped = TRUE, hover = TRUE)
    
    output$texte_evo <- renderUI({
      text <- paste0("<p>",
        generate_texte("Évolution des naissances de"),
        ".</p>")
      return(HTML(text))
    })
    
    data_evo <- reactive({
      data_nat %>% 
        filter(annee %in% periode()) %>% 
        group_by(annee) %>% 
        mutate(n_annee = sum(n)) %>% 
        ungroup %>% 
        filter(
          prenom %in% prenoms_choisis(),
          sexe %in% sexe_prenom()
        ) %>% 
        mutate(prenom = input$prenom) %>% 
        group_by(annee, prenom) %>% 
        summarise(
          n = sum(n),
          n_annee = first(n_annee),
          `%` = round(n / n_annee * 100, 3)
        )
    })
    
    output$graph_evo <- renderG2({
      if (input$prenom == "") return(NULL)
      if(nrow(data_evo()) == 0) return(NULL)
      
      if (input$graph_evo_type == "n") {
        var <- sym("n")
        y_title <- "Nombre de naissances"
      }
      if (input$graph_evo_type == "prop") {
        var <- sym("%")
        y_title <- "Pourcentage des naissances"
      }
      
      g2(data_evo(), asp(x = annee, y = !!var, color = prenom)) %>% 
        fig_line() %>% 
        gauge_x_linear(title = "Années", nice = FALSE) %>% 
        gauge_y_linear(title = y_title, min = 0) %>% 
        conf_legend(prenom, FALSE)
      
    })
    
    output$texte_carte <- renderUI({
      text <- paste0("<p>",
        generate_texte("Répartition des naissances de"),
        ".</p>")
      return(HTML(text))
    })
    
    data_carte <- reactive({
      data_dpt %>% 
        filter(annee %in% periode()) %>% 
        add_count(dpt, name = "naissances_dpt") %>% 
        filter(prenom %in% prenoms_choisis(),
          sexe %in% sexe_prenom()) %>% 
        group_by(dpt) %>% 
        summarise(
          nb = sum(n),
          prop = sum(n) / sum(naissances_dpt) * 100
        ) %>% 
        select(dpt, prop, nb)
    })
    output$graph_carte <- renderLeaflet({
      if (input$prenom == "") return(NULL)
      if(nrow(data_carte()) == 0) return(NULL)
      leaflet_dpt(data_carte())
    })
    
}
