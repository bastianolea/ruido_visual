# funciones que generan el flujo de texto de la app.
# cada modo sigue la lógica de los loops de ascii_rayas.R: en el script original
# cada iteración del loop hacía cat() de un fragmento de texto, y acá cada
# iteración equivale a un elemento del vector de fragmentos que se retorna.

# números al azar ----
# entrega números enteros al azar entre un mínimo y un máximo (ambos incluidos).
# evita el comportamiento de sample() cuando recibe un solo número,
# y tolera que el mínimo y el máximo vengan invertidos
numero_al_azar <- function(cantidad, minimo, maximo) {
  limites <- sort(round(c(minimo, maximo)))
  limites <- pmax(limites, 0)
  valores_posibles <- seq(limites[1], limites[2])

  posiciones <- sample.int(length(valores_posibles), cantidad, replace = TRUE)

  valores_posibles[posiciones]
}


# generador ----
# retorna un vector de `n` fragmentos de texto, que al unirse forman el flujo.
# `contador` es la cantidad de fragmentos generados anteriormente: como los
# patrones dependen de la posición de cada fragmento en la secuencia completa,
# esto permite pedir pocos fragmentos a la vez sin que el patrón se interrumpa
generador <- function(
  n = 10,
  modo = 8,
  simbolo = "|",
  vacio = " ",
  rng_min = 1,
  rng_max = 18,
  contador = 0
) {
  # posición de cada fragmento dentro de la secuencia completa
  i <- contador + seq_len(n)

  es_par <- i %% 2 == 0
  ultimo_digito <- i %% 10

  if (modo == 1) {
    # intervalo regular que genera diagonales de vacío por cada 10
    fragmentos <- ifelse(ultimo_digito == 0, vacio, simbolo)
  } else if (modo == 2) {
    # ruido con figuras de vacío continuas, como líneas de zebra
    cantidad <- numero_al_azar(n, 1, 6)
    fragmentos <- ifelse(ultimo_digito == 0, strrep(vacio, cantidad), simbolo)
  } else if (modo == 3) {
    # lluvia regular con fondo vacío
    fragmentos <- ifelse(es_par, simbolo, strrep(vacio, ultimo_digito))
  } else if (modo == 4) {
    # barras diagonales con degradado
    fragmentos <- ifelse(es_par, vacio, strrep(simbolo, ultimo_digito))
  } else if (modo == 5) {
    # cuadrantes con degradado, diagonales crecientes
    fragmentos <- ifelse(es_par, strrep(simbolo, ultimo_digito), vacio)
  } else if (modo == 6) {
    # degradado de rayas y vacío, ambos con largo variable
    fragmentos <- ifelse(
      es_par,
      strrep(vacio, ultimo_digito),
      strrep(simbolo, ultimo_digito)
    )
  } else if (modo == 7) {
    # fondo blanco con rayas aleatorias
    cantidad <- numero_al_azar(n, rng_min, rng_max)
    fragmentos <- ifelse(es_par, vacio, strrep(simbolo, cantidad))
  } else if (modo == 9) {
    # ruido con barras anchas y líneas solas esporádicas
    unos <- rep(1, 10)
    limites <- pmax(sort(round(c(rng_min, rng_max))), 0)
    valores_posibles <- c(unos, seq(limites[1], limites[2]))
    cantidad <- sample(valores_posibles, n, replace = TRUE)
    fragmentos <- ifelse(
      es_par,
      strrep(vacio, cantidad),
      strrep(simbolo, cantidad)
    )
  } else if (modo == 10) {
    # lienzo blanco con puntos esparcidos (poisson lambda = 10)
    cantidad <- rpois(n, 10)
    fragmentos <- ifelse(cantidad %% 2 == 0, vacio, strrep(simbolo, cantidad))
  } else if (modo == 11) {
    # poisson ruidosa con figuras geométricas esporádicas (lambda = 1)
    cantidad <- rpois(n, 1)
    fragmentos <- ifelse(
      cantidad %% 2 == 0,
      strrep(vacio, cantidad),
      strrep(simbolo, cantidad)
    )
  } else if (modo == 12) {
    # barras verticales o diagonales, según el ancho, con largo fijo de 8
    cantidad <- 8
    fragmentos <- ifelse(
      es_par,
      strrep(vacio, cantidad),
      strrep(simbolo, cantidad)
    )
  } else if (modo == 13) {
    # panal: alternancia de - y símbolo
    fragmentos <- ifelse(es_par, "-", simbolo)
  } else if (modo == 14) {
    # secuencia regular de barras y vacío creciente
    fragmentos <- ifelse(
      ultimo_digito %in% c(1, 2, 3),
      paste0(simbolo, strrep(vacio, ultimo_digito)),
      ""
    )
  } else if (modo == 15) {
    # onda triangular: espacios crecientes y decrecientes separados por barras
    secuencia <- c(1:4, 4:1)
    espacios <- secuencia[((i - 1) %% length(secuencia)) + 1]
    fragmentos <- paste0(strrep(vacio, espacios), simbolo)
  } else {
    # modo 8, y cualquier otro número: ruido con barras anchas
    cantidad <- numero_al_azar(n, rng_min, rng_max)
    fragmentos <- ifelse(
      es_par,
      strrep(vacio, cantidad),
      strrep(simbolo, cantidad)
    )
  }

  fragmentos
}


# cortar en líneas ----
# inserta un salto de línea cada vez que se completan `ancho` caracteres, igual
# que el borde de una consola. el corte cae en cualquier punto del texto, por lo
# que cada línea parte donde quedó la anterior, y eso desfasa los patrones.
# `columna` es la cantidad de caracteres que ya tiene la última línea en pantalla
cortar_en_lineas <- function(texto, ancho, columna = 0) {
  if (nchar(texto) == 0) {
    return("")
  }

  caracteres <- strsplit(texto, split = "")[[1]]

  # caracteres que hay en la línea antes de escribir cada caracter nuevo
  caracteres_previos <- columna + seq_along(caracteres) - 1

  # el salto va antes del caracter que ya no cabe en la línea
  linea_llena <- caracteres_previos > 0 & caracteres_previos %% ancho == 0
  caracteres[linea_llena] <- paste0("\n", caracteres[linea_llena])

  paste0(caracteres, collapse = "")
}

# columna en que queda la última línea luego de agregarle caracteres:
# 0 si no hay texto, y entre 1 y `ancho` en los demás casos
calcular_columna <- function(columna, caracteres_nuevos, ancho) {
  total <- columna + caracteres_nuevos

  if (total == 0) {
    return(0)
  }

  ((total - 1) %% ancho) + 1
}
