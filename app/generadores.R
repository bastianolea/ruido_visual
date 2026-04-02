# Genera un chunk de caracteres en memoria para un paso del loop.
# Cada modo replica exactamente la lógica de ascii_rayas.R:
# el loop original hacía cat() de un fragmento por iteración,
# así que esta función genera N fragmentos y los concatena.

generador <- function(n = 300, modo = 8, simbolo = "|", vacio = " ",
                      rng_min = 1, rng_max = 18, contador = 1L) {
  
  chunks <- character(n)
  i_base <- contador
  
  if (modo == 1) {
    # intervalo regular que genera diagonales de vacío por cada 10
    for (j in seq_len(n)) {
      i <- i_base + j
      chunks[j] <- if (i %% 10 == 0) vacio else simbolo
    }
    
  } else if (modo == 2) {
    # ruido con figuras de vacío continuas, como líneas de zebra
    for (j in seq_len(n)) {
      i <- i_base + j
      if (i %% 10 == 0) {
        chunks[j] <- strrep(vacio, sample(1:6, 1))
      } else {
        chunks[j] <- simbolo
      }
    }
    
  } else if (modo == 3) {
    # lluvia regular con fondo vacío
    for (j in seq_len(n)) {
      i <- i_base + j
      n_v <- i %% 10
      chunks[j] <- if (i %% 2 == 1) strrep(vacio, max(n_v, 1L)) else simbolo
    }
    
  } else if (modo == 4) {
    # barras diagonales con degradado
    for (j in seq_len(n)) {
      i <- i_base + j
      n_rep <- i %% 10
      chunks[j] <- if (i %% 2 == 1) strrep(simbolo, max(n_rep, 1L)) else vacio
    }
    
  } else if (modo == 5) {
    # cuadrantes con degradado, diagonales crecientes
    for (j in seq_len(n)) {
      i <- i_base + j
      n_rep <- i %% 10
      chunks[j] <- if (i %% 2 == 0) strrep(simbolo, max(n_rep, 1L)) else vacio
    }
    
  } else if (modo == 6) {
    # degradado rayas y vacío ambos con largo variable
    for (j in seq_len(n)) {
      i <- i_base + j
      n_rep <- max(i %% 10, 1L)
      chunks[j] <- if (i %% 2 == 0) strrep(vacio, n_rep) else strrep(simbolo, n_rep)
    }
    
  } else if (modo == 7) {
    # fondo blanco con rayas aleatorias
    for (j in seq_len(n)) {
      cantidad <- sample(rng_min:rng_max, 1)
      rayas <- strrep(simbolo, cantidad)
      chunks[j] <- if (j %% 2 == 0) vacio else rayas
    }
    
  } else if (modo == 8) {
    # ruido con barras anchas
    for (j in seq_len(n)) {
      cantidad <- sample(rng_min:rng_max, 1)
      chunks[j] <- if (j %% 2 == 0) strrep(vacio, cantidad) else strrep(simbolo, cantidad)
    }
    
  } else if (modo == 9) {
    # ruido con barras anchas y líneas solas esporádicas
    unos <- rep(1L, 10)
    pool <- c(unos, seq(rng_min, rng_max))
    for (j in seq_len(n)) {
      cantidad <- sample(pool, 1)
      chunks[j] <- if (j %% 2 == 0) strrep(vacio, cantidad) else strrep(simbolo, cantidad)
    }
    
  } else if (modo == 10) {
    # lienzo blanco con puntos esparcidos (poisson lambda=10)
    valores <- rpois(n, 10)
    valores[valores < 1L] <- 1L
    for (j in seq_len(n)) {
      k <- valores[j]
      chunks[j] <- if (k %% 2 == 0) vacio else strrep(simbolo, k)
    }
    
  } else if (modo == 11) {
    # poisson ruidosa con figuras geométricas (lambda=1)
    valores <- rpois(n, 1)
    valores[valores < 1L] <- 1L
    for (j in seq_len(n)) {
      k <- valores[j]
      chunks[j] <- if (k %% 2 == 0) strrep(vacio, k) else strrep(simbolo, k)
    }
    
  } else if (modo == 12) {
    # barras verticales/diagonales con ancho fijo de 8
    cantidad <- 8L
    for (j in seq_len(n)) {
      chunks[j] <- if (j %% 2 == 0) strrep(vacio, cantidad) else strrep(simbolo, cantidad)
    }
    
  } else if (modo == 13) {
    # panal: alternancia de - y |
    for (j in seq_len(n)) {
      i <- i_base + j
      chunks[j] <- if (i %% 2 == 0) "-" else simbolo
    }
    
  } else if (modo == 14) {
    # secuencia regular de barras y vacío creciente
    k <- i_base
    for (j in seq_len(n)) {
      k <- k + 1L
      x <- k %% 10
      if (x %in% c(1L, 2L, 3L)) {
        chunks[j] <- paste0(simbolo, strrep(vacio, x))
      } else {
        chunks[j] <- ""
      }
    }
    
  } else if (modo == 15) {
    # onda triangular: espacios crecientes y decrecientes con barras
    x <- 4L
    secuencia <- c(1:x, x:1)
    for (j in seq_len(n)) {
      y <- secuencia[((j - 1L) %% length(secuencia)) + 1L]
      chunks[j] <- paste0(strrep(vacio, y), simbolo)
    }
    
  } else {
    # fallback: modo 8
    for (j in seq_len(n)) {
      cantidad <- sample(rng_min:rng_max, 1)
      chunks[j] <- if (j %% 2 == 0) strrep(vacio, cantidad) else strrep(simbolo, cantidad)
    }
  }
  
  paste0(chunks, collapse = "")
}