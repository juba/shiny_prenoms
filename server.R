server <- function(input, output, session) {
  
    updateSelectizeInput(session, 'prenom', choices = c(NA, liste_prenoms), server = TRUE)  
    updateSelectizeInput(session, 'prenoms_comp1', choices = c(NA, liste_prenoms), server = TRUE)
    updateSelectizeInput(session, 'prenoms_comp2', choices = c(NA, liste_prenoms), server = TRUE)
    updateSelectizeInput(session, 'prenoms_comp3', choices = c(NA, liste_prenoms), server = TRUE)
    updateSelectizeInput(session, 'prenoms_comp4', choices = c(NA, liste_prenoms), server = TRUE)
  

    periode <- reactive({
      c(input$periode[1], input$periode[2])
    })
    
    nb_sexes_prenom <- reactive({
      if (req(input$prenom) == "") return(2)
      sexes_prenoms$n[sexes_prenoms$prenom == input$prenom]
    })
    
    prenoms_similaires <- reactive({
      prenom_ascii <- iconv(input$prenom, to='ASCII//TRANSLIT')
      prenom_ascii <- str_replace_all(prenom_ascii, "(.)\\1", "\\1")
      similaires <- liste_prenoms_ascii == prenom_ascii 
      res <- liste_prenoms[similaires]
      res[res != input$prenom]
    })
    
    output$screen_affine_prenom <- renderUI({
      if (input$prenom == "") return(NULL)
      if (nb_sexes_prenom() == 1 && length(prenoms_similaires()) == 0) return(NULL)
      out <- h3("Précisions sur le prénom choisi")
      if (nb_sexes_prenom() == 2) {
        out <- list(out,
          p("Ce prénom existe pour des naissances des deux sexes."),
          selectInput("sexe",
            NULL,
            choices = select_sexe_choices)
        )
      }
      if (length(prenoms_similaires()) > 0) {
        out <- list(out, 
          p("Des prénoms similaires ont été trouvés. Sélectionnez ceux que vous souhaitez regrouper avec le prénom initial."),
          list(checkboxGroupInput(
            "prenoms_similaires",
            NULL,
            choices = prenoms_similaires()
          )
        ))
      }
      tagList(out)
    })
    outputOptions(output, "screen_affine_prenom", suspendWhenHidden = FALSE, priority = 1000)
    
    observeEvent(input$sexe, {
      updatePrettyRadioButtons(
        session = session,
        inputId = "tab_pop_type",
        selected = input$sexe
      )
    }, ignoreNULL = FALSE)
    
    sexe_prenom <- reactive({
      if (nb_sexes_prenom() < 2 || req(input$sexe) == "both") return(c("M", "F"))
      input$sexe
    })

    prenoms_choisis <- reactive({
      if (length(input$prenoms_similaires) == 0) return(input$prenom)
      c(input$prenom, input$prenoms_similaires)
    })
    
    # output$screen_test_prenoms_periode <- renderUI({
    #   tmp <- data_nat %>% 
    #     filter(
    #       prenom %in% prenoms_choisis(),
    #       sexe %in% sexe_prenom(),
    #       annee %in% periode()
    #     )
    #   if (nrow(tmp) > 0) return(NULL)
    #   p("Le prénom choisi n'existe pas dans la base à cette période.")
    # })
    # outputOptions(output, "screen_test_prenoms_periode", suspendWhenHidden = FALSE, priority = 1000)
    

    ## OUTPUTS PRÉNOM SEUL ---------------------------------
    
    ## Légendes
    
    label_prenom <- reactive({
      text_sexe <- ""
      if (!is.null(input$sexe) && input$sexe != "both") {
        text_sexe <- glue(" ({input$sexe})")
      }
      glue("<strong>{input$prenom} {text_sexe}</strong>")
    })
    
    label_periode <- reactive({
      if (periode()[1] == periode()[2]) {
        return(glue(" en {periode()[1]}"))
      } else {
        return(glue(" de {input$periode[1]} à {input$periode[2]}"))
      }
    })
    
    output$legende_pop <- renderUI({
      HTML(glue("Popularité de {label_prenom()} {label_periode()}"))
    })
    
    output$legende_evo <- renderUI({
      HTML(glue("Évolution des naissances de {label_prenom()} {label_periode()}"))
    })

    output$legende_carte <- renderUI({
      HTML(glue("Proportion des naissances de {label_prenom()} par département {label_periode()}"))
    })
    
    ## DONNÉES
    
    empty_line <- data.table(
      Classement = NA, `Prénom` = "...", 
      Sexe = "...", Naissances = NA, `%` = NA
    )
    
    data_nat_periode <- reactive({
      data_nat[annee %between% periode()]
    })
    
    data_dpt_periode <- reactive({
      data_dpt[annee %between% periode()]
    })
    
    regroupe_prenoms_choisis <- function(data) {
      data[prenom %chin% prenoms_choisis(), prenom := input$prenom]
    }
    
    ## Données popularité
    
    data_pop <- reactive({
      
      if (input$prenom == "") return(NULL)
      
      d <- data_nat_periode()
      
      if (input$tab_pop_type != "both") {
        d <- d[sexe == input$tab_pop_type]
      }
      
      regroupe_prenoms_choisis(d)
      d <- d[, .(n = sum(n)), by = .(prenom, sexe)]
      d[, n_total := sum(n)]
      d[, `%` := (n / n_total * 100), by = .(prenom, sexe)]
      d <- d[prenom != "_PRENOMS_RARES"]
      setorderv(d, "n", order = -1L)
      d[, Classement := 1:.N]
      d <- d[, .(Classement, prenom, sexe, n, `%`)]
      setnames(d, c("prenom", "sexe", "n"),
                    c("Prénom", "Sexe", "Naissances"))

      classement_prenom <- d[Prénom == input$prenom, Classement]
      tab <- rbind(d[1:10], empty_line, use.names = FALSE)
      if (length(classement_prenom) == 0) {
        tab <- rbind(tab,
          data.table(Classement = NA, `Prénom` = input$prenom, 
            Sexe ="...", Naissances = 0L, `%` = NA),
          use.names = FALSE)
      }
      else if (!all(classement_prenom %in% 1:10)) {
        rows_prenom <- d[classement_prenom]
        for (i in 1:nrow(rows_prenom)) {
          l <- list(tab,
                    rows_prenom[i],
                    empty_line)
          tab <- rbindlist(l, use.names = FALSE)
        }
      }
      tab[, Naissances := prettyNum(Naissances, HTML("&thinsp;"))]
      tab[, Naissances := ifelse(Naissances == "NA", "", Naissances)]
    
      tab[Prénom == input$prenom, Prénom := glue("<strong>{Prénom}</strong>")]
    })
    
    
    ## Données évolution
    
    data_evo <- reactive({
      
      if(input$prenom == "") return(NULL)
      
      d <- data_nat_periode()
      d <- d[prenom %chin% prenoms_choisis()][sexe %chin% sexe_prenom()]
      d[, prenom := input$prenom]
      d <- d[, .(n = sum(n), n_annee = data.table::first(n_annee)), by = .(annee, prenom)]
      d[, `%` := round(n / n_annee * 100, 4)]
      tidyr::complete(d, 
        prenom, annee = seq(periode()[1], periode()[2]), 
        fill = list(n = 0, `%` = 0))
    })
    
    ## Données carte par département
    
    data_carte <- reactive({
      
      if(input$prenom == "") return(NULL)
      
      d <- data_dpt_periode()
      d[, naissances_dpt := sum(n), by = dpt]
      d <- d[prenom %chin% prenoms_choisis()][sexe %chin% sexe_prenom()]
      d <- d[, .(n = sum(n), naissances_dpt = data.table::first(naissances_dpt)), by = dpt]
      d[, `%` := n / naissances_dpt * 100]
      d <- d[, .(dpt, `%`, n)]
      tidyr::complete(d,
        dpt = departements$dpt, 
        fill = list(n = NA, `%` = NA))
    })
    
    
    ## SORTIES
    
    
    ## Tableau popularité
    
    output$tab_pop <- renderTable({
      data_pop()
    }, striped = TRUE, hover = TRUE, na = "", 
       sanitize.text.function = function(x) x,
       align = "???r?")
    
    
    ## Graphique évolution
    
    output$graph_evo <- renderG2({
      if (input$prenom == "") return(NULL)
      if (nrow(data_evo()) == 0) return(NULL)
      
      if (input$graph_evo_type == "n") {
        var <- sym("n")
        y_title <- "Nombre de naissances"
        tooltip_template <- '<li>{name}: {value}</li>'
      }
      if (input$graph_evo_type == "prop") {
        var <- sym("%")
        y_title <- "Pourcentage des naissances"
        tooltip_template <- '<li>{name}: {value}%</li>'
      }
      if (periode()[1] == periode()[2]) {
        tmp <- data_evo() %>% mutate(annee = as.character(annee))
        g <- g2(tmp, asp(x = annee, y = !!var, color = prenom)) %>% 
          fig_interval() %>% 
          gauge_x_discrete(title = "Années", nice = TRUE)
      } else {
        g <- g2(data_evo(), asp(x = annee, y = !!var, color = prenom)) %>% 
          fig_line() %>% 
          gauge_x_linear(title = "Années", nice = FALSE)
      } 
      
      g %>% 
        gauge_y_linear(title = y_title, min = 0) %>% 
        conf_tooltip(itemTpl = tooltip_template) %>% 
        conf_legend(prenom, FALSE)
      
    })
    
    ## Carte département
    
    output$graph_carte <- renderLeaflet({
      if (input$prenom == "") return(NULL)
      if(nrow(data_carte()) == 0) return(NULL)
      
      leaflet_dpt(data_carte())
    })

    
    ## OUTPUTS COMPARAISON ---------------------------------
    
    ## Observers données précédentes
    
    observeEvent(input$sexe, {
      updateSelectInput(
        session = session,
        inputId = "sexe_comp1",
        selected = input$sexe
      )
    }, ignoreNULL = FALSE)
    
    observe({
      updateSelectizeInput(
        session = session,
        inputId = "prenoms_comp1",
        selected = prenoms_choisis(),
        choices = c(NA, liste_prenoms),
        server = TRUE
      )
    })
    
    observeEvent(input$periode, {
      updateSliderInput(
        session = session,
        inputId = "periode_comp",
        value = input$periode
      )
    })
    
    ## Légendes
    
    periode_comp <- reactive({
      c(input$periode_comp[1], input$periode_comp[2])
    })
    
    label_periode_comp <- reactive({
      if (periode_comp()[1] == periode_comp()[2]) {
        return(glue(" en {periode_comp()[1]}"))
      } else {
        return(glue(" de {input$periode_comp[1]} à {input$periode_comp[2]}"))
      }
    })
    
    output$legende_evo_comp <- renderUI({
      HTML(glue("Comparaison du nombre de naissances {label_periode_comp()}"))
    })
    
    output$legende_carte_comp <- renderUI({
      HTML(glue("Comparaison de la répartition des naissances {label_periode_comp()}"))
    })
    
    ## Données
    
    prenoms_comp_indices <- reactive({
      purrr::keep(1:4, ~{
        val_x <- input[[glue("prenoms_comp{.x}")]]
        !is.null(val_x) && val_x != ""
      })
    })
    
    label_prenom_comp <- function(prenoms, sexe) {
      text_sexe <- ""
      if (length(sexe) == 1) {
        text_sexe <- glue(" ({sexe})")
      }
      glue("{prenoms[1]}{text_sexe}")
    }
    
    sexe_prenom_comp <- function(i) {
      sexe <- input[[glue("sexe_comp{i}")]]
      if (sexe == "both") return(c("M", "F"))
      sexe
    }

    ## Données comparaison évolution
    
    data_evo_comp <- reactive({
      
      if (length(prenoms_comp_indices()) == 0) return(NULL)
      
      d <- data_nat[annee %between% periode_comp()]
      l <- purrr::map(prenoms_comp_indices(), function(i) {
        prenoms <- input[[glue("prenoms_comp{i}")]]
        tmp_sexe <- sexe_prenom_comp(i)
        tmp <- d[prenom %chin% prenoms][sexe %chin% tmp_sexe]
        label <- label_prenom_comp(prenoms, tmp_sexe)
        tmp[, prenom := label]
        tmp <- tmp[, .(n = sum(n), n_annee = data.table::first(n_annee)), by = .(annee, prenom)]
        tmp[, `%` := round(n / n_annee * 100, 4)]
        tmp <- tmp[, .(prenom, annee, n, `%`)]
        tidyr::complete(tmp,
          prenom = label, annee = seq(periode_comp()[1], periode_comp()[2]), 
          fill = list(n = 0, `%` = 0))  
      })
      rbindlist(l)

    })
    
    ## Données comparaison carte
    
    data_carte_comp <- reactive({
      
      if (length(prenoms_comp_indices()) == 0) return(NULL)
      
      d <- data_dpt[annee %between% periode_comp()]
      d[, naissances_dpt := sum(n), by = dpt]
      
      l <- purrr::map(prenoms_comp_indices(), function(i) {
        prenoms <- input[[glue("prenoms_comp{i}")]]
        sexe <- sexe_prenom_comp(i)
        tmp <- d[prenom %chin% prenoms][sexe %chin% sexe]
        label <- label_prenom_comp(prenoms, sexe)
        tmp[, prenom := label]
        tmp <- tmp[, .(n = sum(n), naissances_dpt = data.table::first(naissances_dpt)), by = .(dpt, prenom)]
        tmp[, `%` := n / naissances_dpt * 100]
        tmp <- tmp[, .(prenom, dpt, n, `%`)]
        tidyr::complete(tmp,
          prenom = label, dpt = departements$dpt, 
          fill = list(n = 0, `%` = 0))  
      })
      
      rbindlist(l)

    })
    
    ## SORTIES COMPARAISON
    
    ## Graphique comparaison évolution
    
    output$graph_evo_comp <- renderG2({

      if (length(prenoms_comp_indices()) == 0) return(NULL)
      if (nrow(data_evo_comp()) == 0) return(NULL)
      
      if (input$graph_evo_comp_type == "n") {
        var <- sym("n")
        y_title <- "Nombre de naissances"
        tooltip_template <- '<li>{name}: {value}</li>'
      }
      if (input$graph_evo_comp_type == "prop") {
        var <- sym("%")
        y_title <- "Pourcentage des naissances"
        tooltip_template <- '<li>{name}: {value}%</li>'
      }
      
      if (periode_comp()[1] == periode_comp()[2]) {
        tmp <- data_evo_comp() %>% mutate(annee = as.character(annee))
        g <- g2(tmp, asp(x = annee, y = !!var, color = prenom)) %>% 
          fig_interval(adjust("dodge")) %>% 
          gauge_x_discrete(title = "Années", nice = FALSE)
      } else {
        g <- g2(data_evo_comp(), asp(x = annee, y = !!var, color = prenom)) %>% 
          fig_line() %>% 
          gauge_x_linear(title = "Années", nice = FALSE)
      }
      
      g %>% 
        gauge_y_linear(title = y_title, min = 0) %>% 
        conf_tooltip(itemTpl = tooltip_template) %>% 
        conf_legend(prenom, position = "right")
    })
    
    
    ## Carte comparaison
    
    output$graph_carte_comp <- renderLeaflet({
      
      if (length(prenoms_comp_indices()) == 0) return(NULL)
      if (nrow(data_carte_comp()) == 0) return(NULL)

      leaflet_dpt_comp(data_carte_comp())
      
    })
    
    
        
}
