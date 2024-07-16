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
        textAreaInput(inputId = "criteria", label = "Kriteria", height = "100px", value = "Kriteria 1, Kriteria 2, Kriteria 3")
      ),
      layout_columns(
        card(
          card_header(class = "bg-dark", "Tabel Standar Nilai Pembandingan"),
          datatable(table_pembanding_df, options = list(dom = 't', ordering=F), selection = 'none', rownames = FALSE)
        ),
        layout_columns(
          card(
            card_header("Tabel Perbandingan Kriteria"),
            max_height = 300,
            full_screen = T,
            card_body(
              DTOutput("dynamicTableCriteria")
            )
          ),
          card(
            card_header("Preferensi Kriteria"),
            full_screen = T,
            uiOutput("sliderCriterias")
          ),
          col_widths = c(12, 12)
        ),
        col_widths = c(4, 8)
      )
    )
  ),
  nav_panel(
    title = "Alternatif",
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        textAreaInput(inputId = "alternatives", label = "Alternatif", height = "100px", value = "Alternatif 1, Alternatif 2, Alternatif 3"),
        actionButton("update_sliders", "Update tabel")
      ),
      layout_columns(
        card(
          card_header(class = "bg-dark", "Tabel Standar Nilai Pembandingan"),
          datatable(table_pembanding_df, options = list(dom = 't', ordering=F), selection = 'none', rownames = FALSE)
        ),
        card(
          card_header("Tabel Perbandingan & Preferensi Alternatif"),
          full_screen = T,
          card_body(
            uiOutput("dynamicTablesAndSliders")
          )
        ),
        col_widths = c(4, 8)
      )
    )
  ),
  nav_panel(
    title = "Analisis"
  ),
  nav_panel(
    title = "Tentang"
  )

)
