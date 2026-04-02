library(shiny)
library(bslib)

source("generadores.R", local = TRUE)

anchos <- "100%"

ui <- page_fillable(
  theme = bslib::bs_theme(bg = "#181818",
                          fg = "#E5E5E5", 
                          primary = "#E5E5E5",
                          base_font = "Monaco",
                          font_scale = .6),
  
  # desactivar parpadeo al cargar
  busyIndicatorOptions(fade_opacity = 1),
  
  # css
  tags$style(".form-control {
             font-size:80%;
             padding: .2rem .4rem;
             border: solid 1px #282828;
             }"),
  
  layout_columns(
    gap = 0,
    col_widths = c(2, 10),
    
    card(min_height = "140px", 
         actionLink("iniciar", "iniciar"),
         actionLink("detener", "detener"),
         
         
         numericInput("modo", "modo",
                      value = 8, width = anchos),
         # selectInput("modo", "modo",
         #              choices = c(7, 8), width = anchos),
         
         layout_columns(gap = 20,
                        textInput("simbolo", "símbolo",
                                  value = "|", width = anchos),
                        
                        textInput("vacio", "vacío",
                                  value = " ", width = anchos),
         ),
         
         # numericInput("tiempo", "tiempo",
         #              value = 0.01,
         #              min = 0.001,
         #              max = 0.1,
         #              step = 0.001,
         #              width = anchos),
         
         numericInput("intervalo", "intervalo",
                      value = 40,
                      min = 10,
                      max = 200,
                      step = 10,
                      width = anchos),
         
         numericInput("ancho", "ancho",
                      value = 120,
                      min = 100,
                      max = 500,
                      step = 10,
                      width = anchos),
         
         numericInput("rng_max", "max", 
                      value = 18, width = anchos),
         
         numericInput("rng_min", "min", 
                      value = 1, width = anchos),
         
         actionLink("limpiar", "limpiar"),
    ),
    
    card(
      div(style = css(height = "100%",
                      width = "100%",
                      font_family = "Monaco",
                      font_size = "80%",
                      line_height = 1.1,
                      display = "flex",
                      overflow = "hidden",
                      white_space = "pre",
                      flex_direction = "column-reverse"),
          
          htmlOutput('stream'),
      )
    )
  )
)

server <- function(input, output, session) {
  
  rv <- reactiveValues(
    lineas    = character(0),
    corriendo = FALSE,
    contador  = 0L
  )
  
  # loop de animación en memoria (reemplaza callr + archivo)
  observe({
    if (rv$corriendo) {
      invalidateLater(as.numeric(input$intervalo))
      isolate({
        # obtener ancho del contenedor en caracteres
        # generamos unas pocas líneas por frame
        n_lineas <- 1L
        ancho <- input$ancho
        
        nuevas <- character(n_lineas)
        for (l in seq_len(n_lineas)) {
          rv$contador <- rv$contador + 300L
          
          chunk <- generador(
            n        = 300L,
            modo     = input$modo,
            simbolo  = input$simbolo,
            vacio    = input$vacio,
            rng_min  = input$rng_min,
            rng_max  = input$rng_max,
            contador = rv$contador
          )
          # 
          # # tomar los primeros `ancho` caracteres para formar una línea cuadrada
          # # si el chunk es más corto, rellenar con vacío
          # if (nchar(chunk) >= ancho) {
          #   nuevas[l] <- substr(chunk, 1, ancho)
          # } else {
          #   nuevas[l] <- paste0(chunk, strrep(" ", ancho - nchar(chunk)))
          # }
          nuevas[l] <- chunk
        }
        
        # acumular líneas con límite
        rv$lineas <- c(rv$lineas, nuevas)
        max_lineas <- 50L
        if (length(rv$lineas) > max_lineas) {
          rv$lineas <- tail(rv$lineas, max_lineas)
        }
      })
    }
  })
  
  output$stream <- renderUI({
    texto <- paste0(rv$lineas, collapse = "\n")
    HTML(texto)
  })
  
  observeEvent(input$iniciar, {
    rv$corriendo <- TRUE
  })
  
  observeEvent(input$detener, {
    rv$corriendo <- FALSE
  })
  
  # reiniciar al recibir input
  observeEvent(
    list(input$modo, 
         input$simbolo, input$vacio,
         input$rng_max, input$rng_min),
    {
      rv$lineas <- character(0)
      rv$contador <- 0L
    },
    ignoreInit = TRUE
  )
  
  # limpiar
  observeEvent(input$limpiar, {
    rv$lineas <- character(0)
    rv$contador <- 0L
  })
}

# Run the application 
shinyApp(ui, server)