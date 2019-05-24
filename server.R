server <- function(input, output, session) {
  
    updateSelectizeInput(session, 'prenom', choices = c(NA, liste_prenoms), server = TRUE)  
    updateSelectizeInput(session, 'prenoms_comp1', choices = c(NA, liste_prenoms), server = TRUE)
    updateSelectizeInput(session, 'prenoms_comp2', choices = c(NA, liste_prenoms), server = TRUE)
    updateSelectizeInput(session, 'prenoms_comp3', choices = c(NA, liste_prenoms), server = TRUE)
  

    periode <- reactive({
      seq(input$periode[1], input$periode[2])
    })
    
    nb_sexe_prenom <- reactive({
      if (req(input$prenom) == "") return(2)
      sexes_prenoms$n[sexes_prenoms$prenom == input$prenom]
    })
    
    prenoms_similaires <- reactive({
      prenom_ascii <- iconv(input$prenom, to='ASCII//TRANSLIT')
      prenom_ascii <- str_replace_all(prenom_ascii, "(.)\\1", "\\1")
      res <- liste_prenoms[liste_prenoms_ascii == prenom_ascii]
      res[res != input$prenom]
    })
    
    output$screen_sexe_similaires <- renderUI({
      out <- NULL
      if (nb_sexe_prenom() == 2) {
        out <- list(out,
          selectInput("sexe",
            "Filter sur le sexe :",
            choices = select_sexe_choices)
        )
      }
      if (length(prenoms_similaires()) > 0) {
        out <- c(out, 
          list(checkboxGroupInput(
            "prenoms_similaires",
            "Regrouper avec",
            choices = prenoms_similaires()
          )
        ))
      }
      tagList(out)
    })
    outputOptions(output, "screen_sexe_similaires", suspendWhenHidden = FALSE, priority = 1000)
    
    sexe_prenom <- reactive({
      if (nb_sexe_prenom() < 2 || req(input$sexe) == "both") return(c("M", "F"))
      input$sexe
    })

    prenoms_choisis <- reactive({
      if (length(input$prenoms_similaires) == 0) return(input$prenom)
      c(input$prenom, input$prenoms_similaires)
    })
    
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
    outputOptions(output, "screen_test_prenoms_periode", suspendWhenHidden = FALSE, priority = 1000)
    

    ## OUTPUTS PRÉNOM SEUL ---------------------------------
    
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
    
    
    empty_line <- tibble(
      Classement = NA, Prénom = "...", 
      Sexe = "...", Naissances = NA, `%` = NA
    )
    
    data_pop <- reactive({
      
      if (input$prenom == "") return(NULL)
      
      tmp <- data_nat %>% 
        filter(annee %in% periode())
      
      if (input$tab_pop_type == "M") {
        tmp <- tmp %>% filter(sexe == "M")
      } else if (input$tab_pop_type == "F") {
        tmp <- tmp %>% filter(sexe == "F") 
      }
      
      tmp <- tmp %>% 
        mutate(prenom = if_else(prenom %in% prenoms_choisis(), input$prenom, prenom)) %>% 
        group_by(prenom, sexe) %>% 
        summarise(n = as.integer(sum(n))) %>% 
        ungroup() %>% 
        mutate(n_total = sum(n),
               `%` = round(n / n_total * 100, 3)) %>% 
        filter(prenom != "_PRENOMS_RARES") %>% 
        arrange(desc(n)) %>% 
        mutate(Classement = 1:n()) %>% 
        select(Classement, Prénom = prenom, Sexe = sexe, Naissances = n, `%`)

      classement_prenom <- tmp$Classement[tmp$Prénom == input$prenom]
      tab <- tmp %>% slice(1:10) %>% bind_rows(empty_line)
      if (length(classement_prenom) == 0) {
        tab <- tab %>% 
          bind_rows(tibble(Classement = NA, Prénom = input$prenom, 
            Sexe ="...", Naissances = 0L, `%` = NA))
      }
      else if (!all(classement_prenom %in% 1:10)) {
        tab <- tab %>% 
          slice(1:10) %>% 
          bind_rows(empty_line)
        rows_prenom <- tmp %>% slice(classement_prenom)
        for (i in 1:nrow(rows_prenom)) {
          tab <- tab %>% 
            bind_rows(rows_prenom %>% slice(i)) %>% 
            bind_rows(empty_line)
        }
      }
    
      tab <- tab %>% 
        mutate(Prénom = ifelse(Prénom == input$prenom,
          paste0("<strong>",Prénom,"</strong>"),
          Prénom))
      
      tab
    })
    
    output$tab_pop <- renderTable({
      data_pop()
    }, striped = TRUE, hover = TRUE, na = "", 
       sanitize.text.function = function(x) x)
    
    output$texte_evo <- renderUI({
      text <- paste0("<p>",
        generate_texte("Évolution des naissances de"),
        ".</p>")
      return(HTML(text))
    })
    
    data_evo <- reactive({
      data_nat %>% 
        filter(annee %in% periode(),
          prenom %in% prenoms_choisis(),
          sexe %in% sexe_prenom()
        ) %>% 
        mutate(prenom = input$prenom) %>% 
        group_by(annee, prenom) %>% 
        summarise(
          n = sum(n),
          #n_annee = first(n_annee),
          `%` = round(n / first(n_annee) * 100, 3)
        ) %>% 
        ungroup() %>% 
        tidyr::complete(prenom, annee = periode(), fill = list(n = 0, `%` = 0))
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
      
      if(length(periode()) == 1) {
        tmp <- data_evo() %>% mutate(annee = as.character(annee))
        g <- g2(tmp, asp(x = annee, y = !!var, color = prenom)) %>% 
                fig_interval() %>% 
                gauge_x_discrete(title = "Années", nice = FALSE) %>% 
                gauge_y_linear(title = y_title, min = 0) %>% 
                conf_legend(prenom, FALSE)
        return(g)
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
        group_by(dpt) %>% 
        mutate(naissances_dpt = sum(n)) %>% 
        ungroup() %>% 
        filter(prenom %in% prenoms_choisis(),
               sexe %in% sexe_prenom()) %>% 
        group_by(dpt) %>% 
        summarise(
          nb = sum(n),
          prop = sum(n) / sum(naissances_dpt) * 100
        ) %>% 
        select(dpt, prop, nb) %>% 
        tidyr::complete(dpt = departements$dpt, fill = list(nb = NA, prop = NA))
    })
    
    output$graph_carte <- renderLeaflet({
      if (input$prenom == "") return(NULL)
      if(nrow(data_carte()) == 0) return(NULL)
      leaflet_dpt(data_carte())
    })

    
    ## OUTPUTS COMPARAISON ---------------------------------
    
    prenoms_comparaison <- reactive({
      res <- list(prenoms_choisis())
      for (i in 1:3) {
        tmp <- input[[paste0("prenoms_comp", i)]]
        if (!is.null(tmp) && tmp != "") res <- c(res, list(tmp))
      }
      if (length(res) == 1) return(NULL)
      res
    })
    
    sexes_comparaison <- reactive({
      res <- list(sexe_prenom())
      for (i in 1:3) {
        tmp <- input[[paste0("prenoms_comp", i)]]
        if (!is.null(tmp) && tmp != "") {
          tmp_sexe <- input[[paste0("sexe_comp", i)]]
          if (!is.null(tmp_sexe) && tmp_sexe == "both") {
            res <- c(res, list(c("M", "F")))
          } else if (!is.null(tmp_sexe)) {
            res <- c(res, list(tmp_sexe))
          }
        }
      }
      if (length(res) == 1) return(NULL)
      res  
    })
    
    generate_texte_comp <- function() {
      text <- "Comparaison" 
      if (length(periode()) == 1) {
        text <- paste0(text, " en ", periode())
      } else {
        text <- paste0(text, " de ", input$periode[1], " à ", input$periode[2])
      }
      text
    }
    
    output$texte_evo_comp <- renderUI({
      text <- paste0("<p>",
        generate_texte_comp(),
        ".</p>")
      return(HTML(text))
    })
    
    data_evo_comp <- reactive({
      
      if(is.null(prenoms_comparaison())) return(NULL)
      
      tmp <- data_nat %>% 
        filter(annee %in% periode()) 
      tmp <- purrr::map2_dfr(prenoms_comparaison(), sexes_comparaison(), ~{
        tmp %>% 
          filter(prenom %in% .x,
                 sexe %in% .y) %>% 
          mutate(prenom = .x[1])
      })

      tmp %>% 
        group_by(annee, prenom) %>% 
        summarise(
          n = sum(n),
          `%` = round(n / first(n_annee) * 100, 3)
        ) %>% 
        ungroup() %>% 
        tidyr::complete(prenom, annee = periode(), fill = list(n = 0, `%` = 0))
    })
    
    output$graph_evo_comp <- renderG2({

      if (is.null(prenoms_comparaison())) return(NULL)

      if(nrow(data_evo_comp()) == 0) return(NULL)
      
      if (input$graph_evo_comp_type == "n") {
        var <- sym("n")
        y_title <- "Nombre de naissances"
      }
      if (input$graph_evo_comp_type == "prop") {
        var <- sym("%")
        y_title <- "Pourcentage des naissances"
      }
      
      if(length(periode()) == 1) {
        tmp <- data_evo_comp() %>% ungroup %>% mutate(annee = as.character(annee))
        g <- g2(tmp, asp(x = annee, y = !!var, color = prenom)) %>% 
          fig_interval(adjust("dodge")) %>% 
          gauge_x_discrete(title = "Années", nice = FALSE) %>% 
          gauge_y_linear(title = y_title, min = 0) %>% 
          conf_legend(prenom, position = "right")
        return(g)
      } 
      
      g2(data_evo_comp(), asp(x = annee, y = !!var, color = prenom)) %>% 
        fig_line() %>% 
        gauge_x_linear(title = "Années", nice = FALSE) %>% 
        gauge_y_linear(title = y_title, min = 0) %>% 
        conf_legend(prenom, position = "right")
    })
    
    output$texte_carte_comp <- renderUI({
      text <- paste0("<p>",
        generate_texte_comp(),
        ".</p>")
      return(HTML(text))
    })
    
    data_carte_comp <- reactive({

      if(is.null(prenoms_comparaison())) return(NULL)

      tmp <- data_dpt %>% 
        filter(annee %in% periode()) %>% 
        group_by(dpt) %>% 
        mutate(naissances_dpt = sum(n)) %>%
        ungroup()
      tmp <- purrr::map2_dfr(prenoms_comparaison(), sexes_comparaison(), ~{
        tmp %>% 
          filter(prenom %in% .x,
            sexe %in% .y) %>% 
          mutate(prenom = .x[1])
      })

      tmp %>% 
        group_by(dpt, prenom) %>% 
        summarise(
          nb = sum(n),
          prop = sum(n) / first(naissances_dpt) * 100
        ) %>% 
        ungroup() %>% 
        tidyr::complete(prenom, dpt = departements$dpt, fill = list(nb = NA, prop = NA))

    })
    
    output$graph_carte_comp <- renderLeaflet({
      if(is.null(prenoms_comparaison())) return(NULL)
      if(nrow(data_carte_comp()) == 0) return(NULL)
      print(data_carte_comp())
      leaflet_dpt_comp(data_carte_comp())
    })
    
    
        
}
