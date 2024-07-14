page_navbar(
  id = "main_page",
  theme = bs_theme(
    version = version_default(),
    font_scale = 0.9
  ),
  bg = "#0062cc",
  header = tags$head(
    tags$style(
      HTML("
        #map1{ height: 60px; }
      ")
    )
  ),

  title = "Simulasi AHP",
  nav_panel(
    title = "Kriteria",
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        textInput(inputId = "criteria", label = "Kriteria", value = "Kriteria 1, Kriteria 2, Kriteria 3")
      ),
      card(
        card_header("Tabel Perbandingan Kriteria"),
        max_height = 300,
        full_screen = T,
        card_body(
          DTOutput("dynamicTbl")
        )
      ),
      layout_column_wrap(
        style = css(grid_template_columns = "2fr 1fr"),
        card(
          card_header("Preferensi Kriteria"),
          uiOutput("sliders"),
          br(),
          textOutput("selected_values")
        ),
        card(
          card_header(class = "bg-dark", "Tabel Standar Nilai Pembandingan"),
          datatable(table_pembanding_df, options = list(dom = 't', ordering=F), selection = 'none', rownames = FALSE)
        )
      )

    )
  ),
  nav_panel(
    title = "Alternatif"
  ),
  nav_panel(
    title = "Analisis"
  ),
  nav_panel(
    title = "Tentang"
  )

)
