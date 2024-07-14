function(input, output, session) {
  rv <- reactiveValues(
    list_criteria = "Kriteria 1, Kriteria 2, Kriteria 3",
    criterias = NULL,
    num_criteria = 3,
    matrix_criteria = NULL,
    comb_sets = NULL
  )

  output$dynamicTbl <- renderDT({
    rv$list_criteria <- input$criteria
    rv$criterias <- strsplit(rv$list_criteria, ", ")[[1]]
    criterias <- rv$criterias
    rv$num_criteria <- length(criterias)

    df <- data.frame(matrix(0, nrow = rv$num_criteria, ncol = rv$num_criteria))
    colnames(df) <- criterias
    rownames(df) <- criterias

    diag(df) <- 1
    rv$matrix_critera <- df
    df
  }, options = list(dom = 't', ordering=F), selection = 'none')

  output$sliders <- renderUI({
    criterias <- strsplit(rv$list_criteria, ", ")[[1]]
    num_options <- ((rv$num_criteria-1) ^ 2 + (rv$num_criteria-1)) / 2
    sets <- combinations(num_options, 2, repeats.allowed = F, v=1:num_options)
    print(sets)
    rv$comb_sets <- sets

    # Generate radio buttons based on num_options
    slider_inputs <- lapply(1:num_options, function(i) {
      sliderInput(
        inputId = paste0("nilai_kriteria", i),
        label = paste0(criterias[sets[i, 1]], " vs ", criterias[sets[i, 2]]),
        min = -9,
        max = 9,
        value = 1,
        step = 1,
        width = '100%',
        ticks = T
      )
    })

    # Return the UI elements
    do.call(tagList, slider_inputs)
  })

  # Reactive expression to get the selected values
  selected_values <- reactive({
    sapply(1:rv$num_criteria, function(i) {
      input[[paste0("nilai_kriteria", i)]]
    })
  })

  # Display the selected values
  output$selected_values <- renderText({
    paste("Selected values:", paste(selected_values(), collapse = ", "))
  })

}
