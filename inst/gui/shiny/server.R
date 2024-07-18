function(input, output, session) {
  rv <- reactiveValues(
    criterias = NULL,
    num_criteria = 3,
    comb_sets = NULL,
    mtx_criteria = list(),

    alternatives = NULL,
    num_alternatives = 3,
    comb_sets_alt = NULL,
    mtx_alternative = list(),
  )

  ahp <- reactiveValues(
    VE = NULL,
    VP = NULL,
    VA = NULL,
    VB = NULL,
    Qmax = NULL,
    CI = NULL,
    CR = NULL
  )

  ahpAlt <- reactiveValues(
    VE = NULL,
    VP = NULL,
    VA = NULL,
    VB = NULL,
    Qmax = NULL,
    CI = NULL,
    CR = NULL
  )


  ## SETUP CRITERIA USER INTERFACE ####
  output$sliderCriterias <- renderUI({
    if(!is.null(input$criteria) && input$criteria !=""){
      rv$criterias <- trimws(unlist(strsplit(input$criteria, ",")))
      rv$num_criteria <- length(rv$criterias)
    }

    if(rv$num_criteria > 2){
      num_options <- ((rv$num_criteria-1) ^ 2 + (rv$num_criteria-1)) / 2
      counter <- num_options
      sets <- combinations(rv$num_criteria, 2, repeats.allowed = F, v=1:rv$num_criteria)
      rv$comb_sets <- sets
    } else if(rv$num_criteria == 2) {
      counter <- rv$num_criteria - 1
      sets <- combinations(rv$num_criteria, 2, repeats.allowed = F, v=1:rv$num_criteria)
      rv$comb_sets <- sets
    } else {
      rv$comb_sets <- NULL
    }

    validate(
      need(!is.null(rv$criterias) && rv$num_criteria >= 2, "Minimal dua kriteria diperlukan")
    )

    # Generate radio buttons based on num_options
    slider_inputs <- lapply(1:counter, function(i) {
      sliderInput(
        inputId = paste0("nilai_kriteria_", i),
        label = paste0(rv$criterias[sets[i, 1]], " vs ", rv$criterias[sets[i, 2]]),
        min = -9,
        max = 9,
        value = 0,
        step = 1,
        width = '100%',
        ticks = T
      )
    })

    # Return the UI elements
    do.call(tagList, slider_inputs)
  })

  observeEvent(input$update_criteria, {
    df <- data.frame(matrix(0, nrow = rv$num_criteria, ncol = rv$num_criteria))
    colnames(df) <- rv$criterias
    rownames(df) <- rv$criterias

    sets <- rv$comb_sets
    diag(df) <- 1
    rv$mtx_criteria <- df

    for(i in 1:rv$num_criteria){
      nilai <- input[[paste0("nilai_kriteria_", i)]]

      if(nilai != 0 && !is.null(nilai) && is.numeric(nilai)) {
        if(nilai < 0 ){
          df[sets[i, 1], sets[i, 2]] <- 1 / abs(nilai)
          df[sets[i, 2], sets[i, 1]] <- abs(nilai)
        } else {
          df[sets[i, 1], sets[i, 2]] <- nilai
          df[sets[i, 2], sets[i, 1]] <- 1 / nilai
        }
      }
    }

    rv$mtx_criteria <- df
    output$dynamicTableCriteria <- renderDT({
      rv$mtx_criteria
    }, options = list(dom = 't', ordering=F), selection = 'none')
  })

  ## SETUP ALTERNATIVES USER INTERFACE ####
  output$dynamicTablesAndSliders <- renderUI({
    if(!is.null(input$alternatives) && input$alternatives !=""){
      rv$alternatives <- trimws(unlist(strsplit(input$alternatives, ",")))
      rv$num_alternatives <- length(rv$alternatives)
    }

    if(rv$num_alternatives > 2){
      num_options <- ((rv$num_alternatives-1) ^ 2 + (rv$num_alternatives-1)) / 2
      counter <- num_options
      sets <- combinations(rv$num_alternatives, 2, repeats.allowed = F, v=1:rv$num_alternatives)
      rv$comb_sets_alt <- sets
    } else if(rv$num_alternatives == 2) {
      counter <- rv$num_alternatives - 1
      sets <- combinations(rv$num_alternatives, 2, repeats.allowed = F, v=1:rv$num_alternatives)
      rv$comb_sets_alt <- sets
    } else {
      rv$comb_sets_alt <- NULL
    }

    validate(
      need(!is.null(rv$alternatives) && rv$num_alternatives >= 2, "Minimal dua alternatif diperlukan")
    )

    tables_and_sliders <- lapply(1:rv$num_criteria, function(i){
      tagList(
        h5(paste0("Kriteria ", i, ": ", rv$criterias[i])),
        DTOutput(paste0("tbl_alt_", i)),
        lapply(1:counter, function(j){
          sliderInput(
            inputId = paste0("slider_alt_", i, "_", j),
            label = paste0(rv$alternatives[sets[j, 1]], " vs ", rv$alternatives[sets[j, 2]]),
            min = -9,
            max = 9,
            value = 0,
            step = 1,
            width = '100%',
            ticks = T
          )
        }),
        br()
      )
    })

    # Return the UI elements
    do.call(tagList, tables_and_sliders)
  })

  observeEvent(input$update_alternative, {
    df <- matrix(0, nrow = rv$num_alternatives, ncol = rv$num_alternatives)
    colnames(df) <- rv$alternatives
    rownames(df) <- rv$alternatives
    diag(df) <- 1
    sets <- rv$comb_sets_alt
    req(sets)

    lapply(1:rv$num_criteria, function(i){
      rv$mtx_alternative[[paste0("tbl_", i)]] <- df

      for (j in 1:rv$num_alternatives) {
        slider_id <- paste0("slider_alt_", i, "_", j)
        nilai <- input[[slider_id]]

        if(nilai != 0 && !is.null(nilai) && is.numeric(nilai)) {
          if(nilai < 0 ){
            df[sets[j, 1], sets[j, 2]] <- 1 / abs(nilai)
            df[sets[j, 2], sets[j, 1]] <- abs(nilai)
          } else {
            df[sets[j, 1], sets[j, 2]] <- nilai
            df[sets[j, 2], sets[j, 1]] <- 1 / nilai
          }
        }
      }

      # print(rv$alternatives[i])
      rv$mtx_alternative[[paste0("tbl_", i)]] <- df
      # print(rv$mtx_alternative[[paste0("tbl_", i)]])

      output[[paste0("tbl_alt_", i)]] <- renderDT({
        rv$mtx_alternative[[paste0("tbl_", i)]]
      }, options = list(dom = 't', ordering = FALSE), selection = 'none')
    })
  })

  criteria_res <- reactive({
    req(rv$mtx_criteria)
    df <- rv$mtx_criteria
    num_criteria <- rv$num_criteria

    ahp$VE <- apply(df, 1, calculateVE, n=num_criteria)
    ahp$VP <- calculateVP(ahp$VE)
    ahp$VA <- as.matrix(df) %*% ahp$VP
    ahp$VB <- ahp$VA / ahp$VP
    ahp$Qmax <- sum(ahp$VB) / num_criteria
    ahp$CI <- (ahp$Qmax / num_criteria) / (num_criteria - 1)
    ahp$CR <- ahp$CI / RI(num_criteria)

    r <- round(ahp$VP*100, 2)
    res <- data.frame(Bobot=r)
    res
  })

  output$bobotKriteria <- renderDT({
    df <- criteria_res()
    df
  }, options = list(dom = 't', ordering = FALSE), selection = 'none')


  output$boxCriteria <- renderUI({
    tagList(
      p(
        "CI: ", round(ahp$CI, 4)
      ),
      p(
        "CR: ", round(ahp$CR, 4)
      )
    )
  })

  alternative_res <- reactive({
    req(rv$mtx_alternative)
    df <- rv$mtx_alternative
    num_alts <- rv$num_alternatives

    for(i in 1:rv$num_criteria){
      df_ <- df[[paste0("tbl_", i)]]
      ahpAlt$VE[[i]] <- apply(df_, 1, calculateVE, n=num_alts)
      ahpAlt$VP[[i]] <- calculateVP(ahpAlt$VE[[i]])
      ahpAlt$VA[[i]] <- as.matrix(df_) %*% ahpAlt$VP[[i]]
      ahpAlt$VB[[i]] <- ahpAlt$VA[[i]] / ahpAlt$VP[[i]]
      ahpAlt$Qmax[[i]] <- sum(ahpAlt$VB[[i]]) / num_alts
      ahpAlt$CI[[i]] <- (ahpAlt$Qmax[[i]] / num_alts) / (num_alts - 1)
      ahpAlt$CR[[i]] <- ahpAlt$CI[[i]] / RI(num_alts)
    }

    goals <- data.frame(matrix(unlist(ahpAlt$VP), nrow=length(ahpAlt$VP), byrow=TRUE))
    r <- round(t(goals) %*% ahp$VP*100, 2)
    fin <- data.frame(Bobot=r)
    rownames(fin) <- rv$alternatives

    fin
  })

  output$bobotAlternative <- renderDT({
    df <- alternative_res()
    df
  }, options = list(dom = 't', ordering = FALSE), selection = 'none')

  output$boxAlternative <- renderUI({
    lapply(1:rv$num_criteria, function(i){
      tagList(
        p("Rasio Kekonsistenan terhadap ", rv$criterias[i], ": ", round(ahpAlt$CR[[i]], 4))
      )
    })

  })

}
