library(shiny)
library(bslib)

source("generadores.R", local = TRUE)

# cantidad máxima de líneas que se mantienen en memoria y en el navegador
lineas_maximas <- 300

# tope de fragmentos por actualización, para evitar ráfagas luego de una pausa
fragmentos_maximos <- 500

# el navegador solamente agrega al final el texto que le envía R,
# y descarta las líneas más antiguas
script_flujo <- "
const nodo_flujo = () => document.getElementById('stream');

Shiny.addCustomMessageHandler('agregar_texto', (mensaje) => {
  const nodo = nodo_flujo();
  const texto = nodo.firstChild ?? nodo.appendChild(document.createTextNode(''));
  texto.appendData(mensaje.texto);

  // recortar desde arriba, siempre en un salto de línea
  if (texto.length > mensaje.limite) {
    const corte = texto.data.indexOf('\\n', texto.length - mensaje.limite);
    texto.deleteData(0, corte + 1);
  }
});

Shiny.addCustomMessageHandler('reemplazar_texto', (mensaje) => {
  nodo_flujo().textContent = mensaje.texto;
});
"

ui <- page_fillable(
  # en móvil la app también ocupa solo el alto de la pantalla
  fillable_mobile = TRUE,
  title = "Ruido visual en R",

  theme = bslib::bs_theme(
    bg = "#181818",
    fg = "#E5E5E5",
    primary = "#E5E5E5",
    base_font = "Monaco",
    font_scale = .8
  ),

  # desactivar parpadeo al cargar
  busyIndicatorOptions(fade_opacity = 1),

  # css
  tags$style(
    ".form-control {
             font-size:80%;
             padding: .2rem .4rem;
             border: solid 1px #282828;
             }"
  ),

  layout_columns(
    gap = 0,
    col_widths = c(2, 10),
    # en móvil los paneles se apilan: un tercio para controles, dos para el texto
    row_heights = c(1, 2),

    card(
      min_height = "140px",
      layout_columns(
        gap = 20,
        col_widths = breakpoints(xs = c(6, 6)),
        style = css(flex_shrink = 0),
        # que la fila no se estire para ocupar el espacio libre del panel
        fill = FALSE,
        actionLink("iniciar", "iniciar"),
        actionLink("detener", "detener"),
      ),

      numericInput("modo", "modo", value = 8, width = "100%"),
      # selectInput("modo", "modo",
      #              choices = c(7, 8), width = "100%"),

      layout_columns(
        gap = 20,
        # lado a lado también en móvil
        col_widths = breakpoints(xs = c(6, 6)),
        # impedir que se comprima y se monte sobre el input siguiente
        style = css(flex_shrink = 0),
        textInput("simbolo", "símbolo", value = "|", width = "100%"),

        textInput("vacio", "vacío", value = " ", width = "100%"),
      ),

      # segundos entre cada fragmento de texto
      numericInput(
        "tiempo",
        "tiempo",
        value = 0.005,
        min = 0.0001,
        max = 0.1,
        step = 0.001,
        width = "100%"
      ),

      # milisegundos entre cada actualización de la pantalla
      numericInput(
        "intervalo",
        "intervalo",
        value = 40,
        min = 10,
        max = 200,
        step = 10,
        width = "100%"
      ),

      # columnas de texto
      numericInput(
        "ancho",
        "ancho",
        value = 128,
        min = 64,
        max = 256,
        step = 16,
        width = "100%"
      ),

      numericInput("rng_max", "max", value = 18, width = "100%"),

      numericInput("rng_min", "min", value = 1, width = "100%"),

      actionLink("limpiar", "limpiar"),
      
      div(style = "font-size: 80%; margin-top: 24px;",
      markdown("Desarrollado en R por [Bastián Olea Herrera](https://bastianolea.rbind.io/). Más información sobre la app [aquí](https://bastianolea.rbind.io/blog/2025-08-27).")
      )
    ),

    card(
      div(
        style = css(
          height = "100%",
          width = "100%",
          font_family = "Monaco, Menlo, Consolas, monospace",
          font_size = "120%",
          line_height = 1.1,
          display = "flex",
          overflow = "hidden",
          white_space = "pre",
          flex_direction = "column-reverse"
        ),

        div(id = "stream"),
      )
    )
  ),

  tags$script(HTML(script_flujo))
)

server <- function(input, output, session) {
  corriendo <- reactiveVal(FALSE)

  # estado del flujo ----
  # variables comunes en vez de reactivas, porque el texto no pasa por
  # un output de shiny sino que se envía directamente al navegador
  flujo <- "" # texto generado, sin saltos de línea
  columna <- 0 # caracteres que tiene la última línea en pantalla
  contador <- 0 # fragmentos generados hasta el momento
  ancho_actual <- 120 # columnas de texto en uso
  ultimo_tick <- as.numeric(Sys.time())

  es_numero <- function(valor) {
    is.numeric(valor) && length(valor) == 1 && !is.na(valor)
  }

  # avanzar el flujo ----
  # genera el texto que corresponde al tiempo transcurrido y lo envía al navegador
  avanzar_flujo <- function() {
    # si algún input está vacío (por ejemplo, mientras se escribe), esperar
    inputs_numericos <- list(
      input$modo,
      input$tiempo,
      input$rng_min,
      input$rng_max
    )

    if (!all(sapply(inputs_numericos, es_numero))) {
      ultimo_tick <<- as.numeric(Sys.time())
      return()
    }

    # cantidad de fragmentos según el tiempo real transcurrido, para que la
    # velocidad del flujo no dependa de la puntualidad de cada actualización
    tiempo <- max(input$tiempo, 0.0001)
    ahora <- as.numeric(Sys.time())
    cantidad_fragmentos <- floor((ahora - ultimo_tick) / tiempo)

    if (cantidad_fragmentos < 1) {
      return()
    }

    if (cantidad_fragmentos > fragmentos_maximos) {
      cantidad_fragmentos <- fragmentos_maximos
      ultimo_tick <<- ahora
    } else {
      # el tiempo sobrante queda a favor de la siguiente actualización
      ultimo_tick <<- ultimo_tick + cantidad_fragmentos * tiempo
    }

    fragmentos <- generador(
      n = cantidad_fragmentos,
      modo = input$modo,
      simbolo = input$simbolo,
      vacio = input$vacio,
      rng_min = input$rng_min,
      rng_max = input$rng_max,
      contador = contador
    )
    contador <<- contador + cantidad_fragmentos

    texto_nuevo <- paste0(fragmentos, collapse = "")

    if (nchar(texto_nuevo) == 0) {
      return()
    }

    # cortar el texto nuevo siguiendo desde donde quedó la última línea
    texto_cortado <- cortar_en_lineas(texto_nuevo, ancho_actual, columna)
    columna <<- calcular_columna(columna, nchar(texto_nuevo), ancho_actual)

    # guardar el texto sin cortar, para poder volver a cortarlo si cambia el ancho
    flujo <<- paste0(flujo, texto_nuevo)
    caracteres_maximos <- lineas_maximas * ancho_actual

    if (nchar(flujo) > caracteres_maximos) {
      flujo <<- substring(flujo, nchar(flujo) - caracteres_maximos + 1)
    }

    session$sendCustomMessage(
      "agregar_texto",
      list(texto = texto_cortado, limite = lineas_maximas * (ancho_actual + 1))
    )
  }

  # loop de animación ----
  observe({
    req(corriendo())

    intervalo <- input$intervalo
    req(es_numero(intervalo))

    invalidateLater(max(intervalo, 10))

    isolate(avanzar_flujo())
  })

  # cambio de ancho ----
  # volver a cortar todo el texto con el ancho nuevo,
  # como cuando se cambia el tamaño de una consola
  observeEvent(input$ancho, {
    req(es_numero(input$ancho))

    ancho_actual <<- min(max(round(input$ancho), 10), 1000)
    columna <<- calcular_columna(0, nchar(flujo), ancho_actual)

    session$sendCustomMessage(
      "reemplazar_texto",
      list(texto = cortar_en_lineas(flujo, ancho_actual))
    )
  })

  observeEvent(input$iniciar, {
    if (!corriendo()) {
      ultimo_tick <<- as.numeric(Sys.time())
    }
    corriendo(TRUE)
  })

  observeEvent(input$detener, {
    corriendo(FALSE)
  })

  # limpiar
  observeEvent(input$limpiar, {
    flujo <<- ""
    columna <<- 0
    contador <<- 0

    session$sendCustomMessage("reemplazar_texto", list(texto = ""))
  })
}

# Run the application
shinyApp(ui, server)
